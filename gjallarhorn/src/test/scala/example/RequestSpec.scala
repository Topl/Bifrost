package example

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.pattern.ask
import _root_.requests.{Requests, RequestsManager}
import akka.util.{ByteString, Timeout}
import attestation.{Address, PublicKeyPropositionCurve25519}
import attestation.AddressEncoder.NetworkPrefix
import crypto.{AssetCode, Box}
import io.circe.{Json, parser}
import keymanager.KeyManager.GenerateKeyFile
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import keymanager.KeyManagerRef
import modifier.BoxId
import settings.NetworkType
import wallet.WalletManager
import wallet.WalletManager._

import scala.reflect.io.Path
import scala.util.{Failure, Success, Try}
import scala.concurrent.{Await, ExecutionContextExecutor}
import scala.collection.mutable.{Map => MMap}
import scala.concurrent.duration._

/**
  * Must be running bifrost with --local and --seed test
  * ex: "run --local --seed test -f"
  */
class RequestSpec extends AsyncFlatSpec
  with Matchers
  with GjallarhornGenerators {

  implicit val actorSystem: ActorSystem = ActorSystem("requestTest", requestConfig)
  implicit val context: ExecutionContextExecutor = actorSystem.dispatcher
  implicit val timeout: Timeout = 30.seconds
  implicit val networkPrefix: NetworkPrefix = 48.toByte //local network

  val keyFileDir: String = requestSettings.application.keyFileDir
  val keyManagerRef: ActorRef = KeyManagerRef("KeyManager", requestSettings.application)

  val bifrostActor: ActorRef = Await.result(actorSystem.actorSelection(
    s"akka.tcp://${requestSettings.application.chainProvider}/user/walletConnectionHandler").resolveOne(), 10.seconds)

  val walletManagerRef: ActorRef = actorSystem.actorOf(
    Props(new WalletManager(keyManagerRef)), name = WalletManager.actorName)

  val requestsManagerRef: ActorRef = actorSystem.actorOf(
    Props(new RequestsManager(bifrostActor)), name = "RequestsManager")
  val requests = new Requests(requestSettings.application, keyManagerRef)

  val path: Path = Path(keyFileDir)
  Try(path.deleteRecursively())
  Try(path.createDirectory())
  val password = "pass"

  val pk1: Address = Await.result((keyManagerRef ? GenerateKeyFile("password", Some("test")))
    .mapTo[Try[Address]], 10.seconds) match {
    case Success(pubKey) => pubKey
    case Failure(ex) => throw new Error(s"An error occurred while creating a new keyfile. $ex")
  }

  val pk2: Address = Await.result((keyManagerRef ? GenerateKeyFile("password2", None))
    .mapTo[Try[Address]], 10.seconds) match {
    case Success(pubKey) => pubKey
    case Failure(ex) => throw new Error(s"An error occurred while creating a new keyfile. $ex")
  }

  val publicKeys: Set[Address] = Set(pk1, pk2)

  val amount = 10
  var transaction: Json = Json.Null
  var signedTransaction: Json = Json.Null
  var newBoxIds: Set[BoxId] = Set()

  walletManagerRef ! ConnectToBifrost(bifrostActor)
  requests.switchOnlineStatus(Some(requestsManagerRef))


  def parseForBoxId(json: Json): Set[BoxId] = {
    val result = (json \\ "result").head
    val newBxs = (result \\ "newBoxes").head.toString()
    parser.decode[List[Box]](newBxs) match {
      case Right(newBoxes) =>
        newBoxes.foreach(newBox => {
          newBoxIds += newBox.id
        })
        newBoxIds
      case Left(e) => sys.error(s"could not parse: $newBxs")
    }
  }


  it should "receive a successful response from Bifrost upon creating asset" in {
    val createAssetRequest: ByteString = ByteString(
      s"""
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
    val transferArbitsRequest: ByteString = ByteString(
      s"""
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
      case Left(error) => throw error
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
    val contains = newBoxIds.map(boxId => result.toString().contains(boxId))
    contains.contains(false) shouldBe false
  }

  //Make sure you re-run bifrost for this to pass.
  it should "update boxes correctly with balance response" in {
    val walletBoxes: MMap[Address, MMap[BoxId, Box]] =
      Await.result((walletManagerRef ? UpdateWallet((balanceResponse \\ "result").head))
      .mapTo[MMap[Address, MMap[BoxId, Box]]], 10.seconds)

    val pk1Boxes: Option[MMap[BoxId, Box]] = walletBoxes.get(pk1)
    pk1Boxes match {
      case Some(map) => assert (map.size === 2)
      case None => sys.error(s"no mapping for given public key: ${pk1.toString}")
    }
    val pk2Boxes: Option[MMap[BoxId, Box]] = walletBoxes.get(pk2)
    pk2Boxes match {
      case Some(map) => assert (map.size === 1)
      case None => sys.error(s"no mapping for given public key: ${pk2.toString}")
    }
  }

  it should "receive a block from bifrost after creating a transaction" in {
    val newBlock: Option[String] = Await.result((walletManagerRef ? GetNewBlock).mapTo[Option[String]], 10.seconds)
    newBlock match {
      case Some(block) => assert(block.contains("timestamp") && block.contains("signature") && block.contains("txId")
        && block.contains("newBoxes"))
      case None => sys.error("no new blocks")
    }
  }


  it should "send msg to bifrost actor when the gjallarhorn app stops" in {
    val bifrostResponse: String = Await.result((walletManagerRef ? GjallarhornStopped).mapTo[String], 100.seconds)
    assert(bifrostResponse.contains("The remote wallet Actor[akka.tcp://requestTest@127.0.0.1") &&
      bifrostResponse.contains("has been removed from the WalletConnectionHandler in Bifrost"))
  }

  /*it should "update wallet correctly after receiving new block" in {
    val block: ByteString = ByteString(
      s"""
         |    [
         |      {
         |        "txType": "PolyTransfer",
         |        "txHash": "G1KX8RPVBBmHWuuZ7ihNkQLXVJa8AMr4DxafAJHUUCuy",
         |        "timestamp": 0,
         |        "signatures": {
         |          "2xdTv8awN1BjgYEw8W1BVXVtiEwG2b29U8KoZQqJrDuEqSQ9e4": "Signature25519(2AXDGYSE4f2sz7tvMMzyHvUfcoJmxudvdhBcmiUSo6ijwfYmfZYsKRxboQMPh3R4kUhXRVdtSXFXMheka4Rc4P2)"
         |        },
         |        "newBoxes": [
         |          {
         |             "nonce": "-5988475187915922381",
         |             "id": "GgNqzkSywewv99vCrb99UakEw1Myn4mqYXo3N4a6PWVW",
         |             "type": "Poly",
         |             "proposition": "3X4AW3Swr1iM1syu2g4Xi4L4eTSJFKxGsZPgVctUYg4ga8MZpD",
         |             "value": "1000000"
         |          },
         |          {
         |             "nonce": "965750754031143229",
         |             "id": "5UGTHuvG7kJVqp9Sw55A1C6wVEtgeQKn12njLG1bbUTK",
         |             "type": "Poly",
         |             "proposition": "4EoSC4YmTm7zoPt5HDJU4aa73Vn2LPrmUszvggAPM5Ff3R1DVt",
         |             "value": "1000000"
         |          },
         |          {
         |             "nonce": "-59884751870915922381",
         |             "id": "GgNqzkSywewv10vCrb99UakEw1Myn5mqYXo3N4a6PWVW",
         |             "type": "Poly",
         |             "proposition": "${pk2.toString}",
         |             "value": "1000000"
         |          }
         |        ],
         |        "to" : [
         |          {
         |            "proposition" : "6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ",
         |            "value" : 0
         |          }
         |        ],
         |        "boxesToRemove": [
         |                        "2fvgQ6xAJbMxtsGv73veyN3sHnwKUh2Lda3b9CyNxriv"
         |        ],
         |        "fee" : 0
         |      }
         |    ]
       """.stripMargin)
    parser.parse(block.utf8String) match {
      case Right(blockJson) =>
        walletManagerRef ! s"new block added: $blockJson"
        Thread.sleep(1000)
        val walletBoxes: MMap[String, MMap[BoxId, Box]] = Await.result((walletManagerRef ? GetWallet)
          .mapTo[MMap[Address, MMap[BoxId, Box]]], 10.seconds)
        val pk1Boxes: Option[MMap[BoxId, Box]] = walletBoxes.get(pk2.toString)
        pk1Boxes match {
          case Some(map) =>
            assert(map.size == 2)
            assert(map.contains("GGDsEQdd5cnbgjKkac9HLpp2joGo6bWgmS2KvhJgd8b8"))
            map.get("GgNqzkSywewv10vCrb99UakEw1Myn5mqYXo3N4a6PWVW") match {
              case Some(json) => assert((json \\ "type").head.toString() == "\"Poly\"")
              case None => sys.error("poly box was not found!")
            }
          case None => sys.error(s"no mapping for given public key: ${pk2.toString}")
        }
      case Left(e) => sys.error(s"Could not parse json $e")
    }
  }*/

}
