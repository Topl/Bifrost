import scala.collection.mutable.{Map => MMap}
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContextExecutor}
import scala.util.{Failure, Success, Try}

/**
 * Must be running bifrost with --local and --seed test
 * ex: "run --local --seed test -f"
 */
class RequestSpec extends AsyncFlatSpec with Matchers with GjallarhornGenerators {

  // define implicit vals
  implicit val actorSystem: ActorSystem = ActorSystem("requestTest", requestConfig)
  implicit val context: ExecutionContextExecutor = actorSystem.dispatcher
  implicit val timeout: Timeout = 30.seconds

  /** Make sure running bifrost in local network! */
  implicit val networkPrefix: NetworkPrefix = 48.toByte

  // set up key manager
  val keyFileDir: String = requestSettings.application.keyFileDir
  val keyManagerRef: ActorRef = KeyManagerRef("KeyManager", requestSettings.application)

  // set up actors
  val chainProvider: ChainProvider = requestSettings.application.defaultChainProviders
    .get(settings.application.currentChainProvider) match {
    case Some(cp) => cp
    case None     => throw new Exception("The current chain provider is not in the list of chain providers!")
  }

  val bifrostActor: ActorRef = Await.result(
    actorSystem
      .actorSelection(s"akka://${chainProvider.chainProvider}/user/walletConnectionHandler")
      .resolveOne(),
    10.seconds
  )

  val walletManagerRef: ActorRef =
    actorSystem.actorOf(Props(new WalletManager(keyManagerRef)), name = WalletManager.actorName)

  val requestsManagerRef: ActorRef =
    actorSystem.actorOf(Props(new RequestsManager(bifrostActor)), name = "RequestsManager")

  val requests = new Requests(requestSettings, keyManagerRef)

  // delete current keys in key file directory
  val path: Path = Path(keyFileDir)
  Try(path.deleteRecursively())
  Try(path.createDirectory())

  // create new keys for testing
  val pk1: Address = Await.result(
    (keyManagerRef ? GenerateKeyFile("password", Some("test")))
      .mapTo[Try[Address]],
    10.seconds
  ) match {
    case Success(pubKey) => pubKey
    case Failure(ex)     => throw new Error(s"An error occurred while creating a new keyfile. $ex")
  }

  val pk2: Address = Await.result(
    (keyManagerRef ? GenerateKeyFile("password2", None))
      .mapTo[Try[Address]],
    10.seconds
  ) match {
    case Success(pubKey) => pubKey
    case Failure(ex)     => throw new Error(s"An error occurred while creating a new keyfile. $ex")
  }
  val publicKeys: Set[Address] = Set(pk1, pk2)

  val amount = 10
  var transaction: Json = Json.Null
  var signedTransaction: Json = Json.Null
  var newBoxIds: Set[BoxId] = Set()

  // set up online mode
  walletManagerRef ! ConnectToBifrost(bifrostActor, chainProvider.networkName)
  requests.switchOnlineStatus(Some(requestsManagerRef))

  /**
   * Helper function for grabbing new box ids
   * @param json json to parse for box ids
   * @return a set of BoxIds
   */
  def parseForBoxId(json: Json): Set[BoxId] = {
    val result = (json \\ "result").head
    val newBxs = (result \\ "newBoxes").head.toString()
    parser.decode[List[Box]](newBxs) match {
      case Right(newBoxes) =>
        newBoxes.foreach(newBox => newBoxIds += newBox.id)
        newBoxIds
      case Left(e) => sys.error(s"could not parse: $newBxs")
    }
  }

  it should "receive a successful response from Bifrost upon creating asset" in {
    val createAssetRequest: ByteString = ByteString(s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "1",
         |   "method": "topl_rawAssetTransfer",
         |   "params": [{
         |     "propositionType": "PublicKeyCurve25519",
         |     "recipients": [
         |            ["$pk1", {
         |                "type": "Asset",
         |                "quantity": $amount,
         |                "assetCode": "${AssetCode(1.toByte, pk1, "test").toString}"
         |              }
         |            ]
         |     ],
         |     "sender": ["$pk1"],
         |     "changeAddress": "$pk1",
         |     "minting": true,
         |     "fee": 1
         |   }]
         |}
       """.stripMargin)
    val tx = requests.sendRequest(createAssetRequest)
    assert(tx.isInstanceOf[Json])
    (tx \\ "error").isEmpty shouldBe true
    (tx \\ "result").head.asObject.isDefined shouldBe true
  }

  it should "receive a successful response from Bifrost upon transfering arbit" in {
    val transferArbitsRequest: ByteString = ByteString(s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "1",
         |   "method": "topl_rawArbitTransfer",
         |   "params": [{
         |     "propositionType": "PublicKeyCurve25519",
         |     "recipients": [["$pk2", $amount]],
         |     "sender": ["$pk1"],
         |     "changeAddress": "$pk1",
         |     "fee": 1,
         |     "data": ""
         |   }]
         |}
       """.stripMargin)
    transaction = requests.sendRequest(transferArbitsRequest)
    newBoxIds = parseForBoxId(transaction)
    assert(transaction.isInstanceOf[Json])
    (transaction \\ "error").isEmpty shouldBe true
    (transaction \\ "result").head.asObject.isDefined shouldBe true
  }

  it should "receive successful JSON response from sign transaction" in {
    val issuer: IndexedSeq[Address] = IndexedSeq(publicKeys.head)
    val response = requests.signTx(transaction, issuer)
    (response \\ "error").isEmpty shouldBe true
    (response \\ "result").head.asObject.isDefined shouldBe true
    signedTransaction = (response \\ "result").head
    assert((signedTransaction \\ "signatures").head.asObject.isDefined)
    val sigs: Map[PublicKeyPropositionCurve25519, Json] =
      (signedTransaction \\ "signatures").head.as[Map[PublicKeyPropositionCurve25519, Json]] match {
        case Left(error)  => throw error
        case Right(value) => value
      }
    val pubKeys = sigs.keySet.map(pubKey => pubKey.address)
    issuer.foreach(key => assert(pubKeys.contains(key)))
    (signedTransaction \\ "tx").nonEmpty shouldBe true
  }

  it should "receive successful JSON response from broadcast transaction" in {
    val response = requests.broadcastTx(signedTransaction)
    assert(response.isInstanceOf[Json])
    (response \\ "error").isEmpty shouldBe true
    (response \\ "result").head.asObject.isDefined shouldBe true
  }

  var balanceResponse: Json = Json.Null

  it should "receive a successful and correct response from Bifrost upon requesting balances" in {
    Thread.sleep(10000)
    balanceResponse = requests.getBalances(publicKeys.map(addr => addr.toString))
    assert(balanceResponse.isInstanceOf[Json])
    (balanceResponse \\ "error").isEmpty shouldBe true

    val result: Json = (balanceResponse \\ "result").head
    result.asObject.isDefined shouldBe true
    (result \\ pk1.toString).nonEmpty shouldBe true
    assert(newBoxIds.forall(boxId => result.toString().contains(boxId.toString)))
  }

  it should "update boxes correctly with balance response" in {
    val walletBoxes: MMap[Address, MMap[BoxId, Box]] =
      Await.result(
        (walletManagerRef ? UpdateWallet((balanceResponse \\ "result").head))
          .mapTo[MMap[Address, MMap[BoxId, Box]]],
        10.seconds
      )

    val pk1Boxes: Option[MMap[BoxId, Box]] = walletBoxes.get(pk1)
    pk1Boxes match {
      case Some(map) => assert(map.size >= 2)
      case None      => sys.error(s"no mapping for given public key: ${pk1.toString}")
    }
    val pk2Boxes: Option[MMap[BoxId, Box]] = walletBoxes.get(pk2)
    pk2Boxes match {
      case Some(map) => assert(map.nonEmpty)
      case None      => sys.error(s"no mapping for given public key: ${pk2.toString}")
    }
  }

  it should "receive a block from bifrost after creating a transaction" in {
    val newBlock: Option[List[Transaction]] = Await.result(
      (walletManagerRef ? GetNewBlock)
        .mapTo[Option[List[Transaction]]],
      10.seconds
    )
    assert(newBlock.isDefined)
  }

  it should "send msg to bifrost actor when the gjallarhorn app stops" in {
    val bifrostResponse: String = Await.result((walletManagerRef ? DisconnectFromBifrost).mapTo[String], 100.seconds)
    assert(
      bifrostResponse.contains("The remote wallet Actor[akka://requestTest@127.0.0.1") &&
      bifrostResponse.contains("has been removed from the WalletConnectionHandler in Bifrost")
    )
  }

}
