import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

/**
  * Must be running bifrost with "--local" and "--seed test"
  * ex: "run --local --seed test -f"
  */
class GjallarhornRPCSpec extends AsyncFlatSpec
  with Matchers
  with GjallarhornGenerators
  with ScalatestRouteTest {

  implicit val timeout: Timeout = Timeout(10.seconds)

  /** Make sure running bifrost in local network! */
  implicit val networkPrefix: NetworkPrefix = 48.toByte

  override def createActorSystem(): ActorSystem = ActorSystem("gjallarhornTest", config)

  //set up key file director and key manager actor
  val keyFileDir: String = settings.application.keyFileDir
  val path: Path = Path(keyFileDir)
  Try(path.deleteRecursively())
  Try(path.createDirectory())
  val keyManagerRef: ActorRef = KeyManagerRef("keyManager", settings.application)

  //generate two keys for testing
  //pk1 should be: 86tS2ExvjGEpS3Ntq5vZgHirUMuee7pJELGD8GmBoUyjXpAaAXTz
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

  //set up WalletManager actor
  val walletManagerRef: ActorRef = system.actorOf(
    Props(new WalletManager(keyManagerRef)), name = WalletManager.actorName)

  val amount = 10

  //Set up api routes
  val requests: Requests = new Requests(settings, keyManagerRef)
  val bifrostApiRoute: ApiRoute =
    GjallarhornOnlineApiRoute(settings.rpcApi, settings.application, keyManagerRef, walletManagerRef, requests)
  val gjalOnlyApiRoute: ApiRoute =
    GjallarhornOfflineApiRoute(settings.rpcApi, settings.application, keyManagerRef, walletManagerRef)
  val route: Route = HttpService(
    Seq(bifrostApiRoute, gjalOnlyApiRoute), settings.rpcApi).compositeRoute

  val httpOrigin: HttpOrigin = HttpOrigin("http://localhost:3000")
  val httpOriginHeader: Origin = Origin(httpOrigin)
  val chainProvider: String = settings.application.defaultChainProviders
    .get(settings.application.currentChainProvider) match {
    case Some(cp) => cp.chainProvider
    case None => "bifrost-client@127.0.0.1:9087"
  }

  /**
    * Method used to create http post request
    * @param jsonRequest the request to send as a ByteString
    * @return the HTTP request
    */
  def httpPOST(jsonRequest: ByteString): HttpRequest = {
    HttpRequest(
      HttpMethods.POST,
      uri = "/",
      entity = HttpEntity(MediaTypes.`application/json`, jsonRequest)
    ).withHeaders(RawHeader("x-api-key", "test_key"))
  }

  it should "successfully connect to Bifrost" in {
    val connectRequest = ByteString(
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "2",
         |   "method": "onlineWallet_connectToBifrost",
         |   "params": [{
         |      "chainProvider": "$chainProvider"
         |   }]
         |}
         """.stripMargin)

    httpPOST(connectRequest) ~> httpOriginHeader ~> route ~> check {
      val responseString = responseAs[String].replace("\\", "")
      parse(responseString.replace("\"{", "{").replace("}\"", "}")) match {
        case Left(f) => throw f
        case Right(res: Json) =>
          assert((res \\ "error").isEmpty)
          assert((res \\ "result").head.asObject.isDefined)
          assert(((res \\ "result").head \\ "connectedToBifrost").head.asBoolean.get)
      }
    }
  }

  val assetCode: AssetCode = AssetCode(1.toByte, pk1, "test")
  var prototypeTx: Json = Map("txType" -> "AssetCreation").asJson
  var msgToSign = ""
  it should "succesfully create an asset offline" in {
    val createAssetRequest = ByteString(
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "2",
         |   "method": "wallet_createRawTransaction",
         |   "params": [{
         |        "txType": "AssetTransfer",
         |        "propositionType": "PublicKeyCurve25519",
         |        "recipients": [["$pk1", $amount]],
         |        "issuer": "$pk1",
         |        "shortName": "test",
         |        "sender": ["$pk1"],
         |        "changeAddress": "$pk1",
         |        "minting": true,
         |        "fee": 1,
         |        "online": false
         |     }]
         |}
       """.stripMargin)

    httpPOST(createAssetRequest) ~> httpOriginHeader ~> route ~> check {
      val responseString = responseAs[String].replace("\\", "")
      parse(responseString.replace("\"{", "{").replace("}\"", "}")) match {
        case Left(f) => throw f
        case Right(res: Json) =>
          assert((res \\ "error").isEmpty)
          prototypeTx = (res \\ "rawTx").head
          msgToSign = (res \\ "messageToSign").head.asString.get
          assert(((res \\ "result").head \\ "rawTx").head.asObject.isDefined)
      }
    }
  }

  var signedTx: Json = Json.Null
  it should "successfully sign a transaction" in {
    val signTxRequest = ByteString(
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "2",
         |   "method": "wallet_signTx",
         |   "params": [{
         |      "signingKeys": ["$pk1"],
         |      "rawTx": $prototypeTx,
         |      "messageToSign": "$msgToSign"
         |   }]
         |}
         """.stripMargin)

    httpPOST(signTxRequest) ~> httpOriginHeader ~> route ~> check {
      val responseString = responseAs[String].replace("\\", "")
      parse(responseString.replace("\"{", "{").replace("}\"", "}")) match {
        case Left(f) => throw f
        case Right(res: Json) =>
          assert((res \\ "error").isEmpty)
          signedTx = ((res \\ "result").head \\ "tx").head
          assert((res \\ "result").head.asObject.isDefined)
      }
    }
  }

  val emptyTx: Null = null
  it should "successfully generate a signature" in {
    val signRequest = ByteString(
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "2",
         |   "method": "wallet_signTx",
         |   "params": [{
         |      "signingKeys": ["$pk2"],
         |      "rawTx": $emptyTx,
         |      "messageToSign": "$msgToSign"
         |   }]
         |}
         """.stripMargin)

    httpPOST(signRequest) ~> httpOriginHeader ~> route ~> check {
      val responseString = responseAs[String].replace("\\", "")
      parse(responseString.replace("\"{", "{").replace("}\"", "}")) match {
        case Left(f) => throw f
        case Right(res: Json) =>
          assert((res \\ "error").isEmpty)
          assert((res \\ "result").head.asObject.isDefined)
          assert(((res \\ "result").head \\ "signatures").head.asObject.isDefined)
      }
    }
  }

  it should "successfully broadcast a tx" in {
    val rqstString =
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "2",
         |   "method": "onlineWallet_broadcastTx",
         |   "params": [{
         |      "method": "topl_broadcastTx",
         |      "params": [{
         |        "tx": $signedTx
         |      }]
         |   }]
         |}
         """.stripMargin
    val rqst = ByteString(rqstString)
    httpPOST(rqst) ~> httpOriginHeader ~> route ~> check {
      val responseString = responseAs[String].replace("\\", "")
      parse(responseString.replace("\"{", "{").replace("}\"", "}")) match {
        case Left(f) => throw f
        case Right(res: Json) =>
          assert((res \\ "error").isEmpty)
          assert((res \\ "result").head.asObject.isDefined)
      }
    }
  }

  it should "succesfully create online arbit tx" in {
    Thread.sleep(10000)
    val transferArbitRequest = ByteString(
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "2",
         |   "method": "onlineWallet_createTransaction",
         |   "params": [{
         |     "method": "topl_rawArbitTransfer",
         |     "params": [{
         |        "propositionType": "PublicKeyCurve25519",
         |        "recipients": [["$pk2", $amount]],
         |        "sender": ["$pk1"],
         |        "changeAddress": "$pk1",
         |        "fee": 1,
         |        "data": "",
         |        "online": true
         |     }]
         |   }]
         |}
       """.stripMargin)

    httpPOST(transferArbitRequest) ~> httpOriginHeader ~> route ~> check {
      val responseString = responseAs[String].replace("\\", "")
      parse(responseString.replace("\"{", "{").replace("}\"", "}")) match {
        case Left(f) => throw f
        case Right(res: Json) =>
          assert((res \\ "error").isEmpty)
          assert((res \\ "result").head.asObject.isDefined)
      }
    }
  }

  it should "successfully create raw poly tx" in {
    val transferPolyRequest = ByteString(
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "2",
         |   "method": "wallet_createRawTransaction",
         |   "params": [{
         |        "txType": "PolyTransfer",
         |        "propositionType": "PublicKeyCurve25519",
         |        "sender": ["$pk1"],
         |        "recipients": [["$pk2", $amount]],
         |        "changeAddress": "$pk1",
         |        "fee": 1,
         |        "data": "",
         |        "online": false
         |     }]
         |}
       """.stripMargin)

    httpPOST(transferPolyRequest) ~> httpOriginHeader ~> route ~> check {
      val responseString = responseAs[String].replace("\\", "")
      parse(responseString.replace("\"{", "{").replace("}\"", "}")) match {
        case Left(f) => throw f
        case Right(res: Json) =>
          assert((res \\ "error").isEmpty)
          assert((res \\ "result").head.asObject.isDefined)
      }
    }
  }

  it should "successfully send online poly tx" in {
    Thread.sleep(10000)
    val transferPolyRequest = ByteString(
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "2",
         |   "method": "onlineWallet_createTransaction",
         |   "params": [{
         |     "method": "topl_rawPolyTransfer",
         |     "params": [{
         |        "propositionType": "PublicKeyCurve25519",
         |        "sender": ["$pk1"],
         |        "recipients": [["$pk2", $amount]],
         |        "changeAddress": "$pk1",
         |        "fee": 1,
         |        "data": "",
         |        "online": true
         |     }]
         |   }]
         |}
       """.stripMargin)

    httpPOST(transferPolyRequest) ~> httpOriginHeader ~> route ~> check {
      val responseString = responseAs[String].replace("\\", "")
      parse(responseString.replace("\"{", "{").replace("}\"", "}")) match {
        case Left(f) => throw f
        case Right(res: Json) =>
          assert((res \\ "error").isEmpty)
          assert((res \\ "result").head.asObject.isDefined)
      }
    }
  }

  it should "get a successful JSON response from balance request" in {
    Thread.sleep(10000)
    val requestBody = ByteString(
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "1",
         |   "method": "wallet_balances",
         |   "params": [{}]
         |}
      """.stripMargin)

      httpPOST(requestBody) ~> httpOriginHeader ~> route ~> check {
        val responseString = responseAs[String].replace("\\", "")
        parse(responseString.replace("\"{", "{").replace("}\"", "}")) match {
          case Left(f) => throw f
          case Right(res: Json) =>
            assert((res \\ "error").isEmpty)

            //pk1 should have fewer polys now
            (((res \\ "result").head \\ pk1.toString).head \\ "PolyBox").head.asNumber.get.toLong match {
              case Some(number) => assert(number < 1000000)
              case None => throw new Error ("balance is not a long")
            }

            //Accounting for tests being run multiple times
            // so tests for amounts being greater than $amount and a multiple of $amount

            //pk1 should have $amount of new asset
            (((res \\ "result").head \\ pk1.toString).head \\ assetCode.toString).head.asNumber.get.toLong match {
              case Some(number) => assert(number >= amount && number % amount == 0)
              case None => throw new Error ("balance is not a long")
            }

            //pk2 should have $amount poly
            (((res \\ "result").head \\ pk2.toString).head \\ "PolyBox").head.asNumber.get.toLong match {
              case Some(number) => assert(number >= amount && number % amount == 0)
              case None => throw new Error ("balance is not a long")
            }

            //pk2 should have $amount arbit
            (((res \\ "result").head \\ pk2.toString).head \\ "ArbitBox").head.asNumber.get.toLong match {
              case Some(number) => assert(number >= amount && number % amount == 0)
              case None => throw new Error ("balance is not a long")
            }

            assert((res \\ "result").head.asObject.isDefined)
        }
      }
    }

  it should "successfully get wallet boxes" in {
    val mnemonicPhraseRequest = ByteString(
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "2",
         |   "method": "wallet_getWalletBoxes",
         |   "params": [{}]
         |}
         """.stripMargin)

    httpPOST(mnemonicPhraseRequest) ~> httpOriginHeader ~> route ~> check {
      val responseString = responseAs[String].replace("\\", "")
      parse(responseString.replace("\"\"", "\"")) match {
        case Left(f) => throw f
        case Right(res: Json) =>
          assert((res \\ "error").isEmpty)
          val result = (res \\ "result").head
          assert (result != null)
      }
    }
  }

  var newAddr: Address = pk2

  it should "successfuly generate a new key and send poly" in {
    val phraseTranslator = Bip39.apply("en")
    val seed = phraseTranslator.uuidSeedPhrase(java.util.UUID.randomUUID.toString)._1
    newAddr = Await.result((keyManagerRef ? GenerateKeyFile("password3", Some(seed)))
      .mapTo[Try[Address]], 12.seconds) match {
        case Success(pubKey) => pubKey
        case Failure(exception) => throw new Error("error creating key file: " + exception)
      }
    val transferPolyRequest = ByteString(
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "2",
         |   "method": "onlineWallet_createTransaction",
         |   "params": [{
         |      "method": "topl_rawPolyTransfer",
         |      "params": [{
         |         "propositionType": "PublicKeyCurve25519",
         |         "sender": ["$pk1"],
         |         "recipients": [["$newAddr", 15]],
         |         "changeAddress": "$pk1",
         |         "fee": 1,
         |         "data": "",
         |         "online": true
         |      }]
         |   }]
         |}
       """.stripMargin)

    httpPOST(transferPolyRequest) ~> httpOriginHeader ~> route ~> check {
      val responseString = responseAs[String].replace("\\", "")
      parse(responseString.replace("\"{", "{").replace("}\"", "}")) match {
        case Left(f) => throw f
        case Right(res: Json) =>
          assert((res \\ "error").isEmpty)
          assert((res \\ "result").head.asObject.isDefined)
      }
    }
  }

  it should "successfully update balance for new key" in {
    Thread.sleep(10000)
    val requestBody = ByteString(
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "1",
         |   "method": "wallet_balances",
         |   "params": [{}]
         |}
      """.stripMargin)

    httpPOST(requestBody) ~> httpOriginHeader ~> route ~> check {
      val responseString = responseAs[String].replace("\\", "")
      parse(responseString.replace("\"{", "{").replace("}\"", "}")) match {
        case Left(f) => throw f
        case Right(res: Json) =>
          assert((res \\ "error").isEmpty)
          assert((res \\ "result").head.asObject.isDefined)
          (((res \\ "result").head \\ newAddr.toString).head \\ "PolyBox").head shouldBe 15.asJson
      }
    }
  }

  var rawPolyTx: Json = Json.Null
  var msgToSignPoly: String = ""
  it should "succesfully create a raw poly tx without bifrost" in {
    val transferPolyRequest = ByteString(
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "2",
         |   "method": "wallet_createRawTransaction",
         |   "params": [{
         |        "txType": "PolyTransfer",
         |        "propositionType": "PublicKeyCurve25519",
         |        "sender": ["$pk1"],
         |        "recipients": [["$pk1", $amount]],
         |        "changeAddress": "$pk1",
         |        "fee": 1,
         |        "online": false
         |   }]
         |}
       """.stripMargin)

    httpPOST(transferPolyRequest) ~> httpOriginHeader ~> route ~> check {
      val responseString = responseAs[String].replace("\\", "")
      parse(responseString.replace("\"{", "{").replace("}\"", "}")) match {
        case Left(f) => throw f
        case Right(res: Json) =>
          assert((res \\ "error").isEmpty)
          rawPolyTx = ((res \\ "result").head \\ "rawTx").head
          msgToSignPoly = ((res \\ "result").head \\ "messageToSign").head.asString.get
          assert(((res \\ "result").head \\ "rawTx").head.asObject.isDefined)
      }
    }
  }

  var signedPolyTx: Json = Json.Null

  it should "successfully sign a transaction created by gjal" in {
    val signTxRequest = ByteString(
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "2",
         |   "method": "wallet_signTx",
         |   "params": [{
         |      "signingKeys": ["$pk1"],
         |      "rawTx": $rawPolyTx,
         |      "messageToSign": "$msgToSignPoly"
         |   }]
         |}
         """.stripMargin)

    httpPOST(signTxRequest) ~> httpOriginHeader ~> route ~> check {
      val responseString = responseAs[String].replace("\\", "")
      parse(responseString.replace("\"{", "{").replace("}\"", "}")) match {
        case Left(f) => throw f
        case Right(res: Json) =>
          assert((res \\ "error").isEmpty)
          signedPolyTx = ((res \\ "result").head \\ "tx").head
          assert((res \\ "result").head.asObject.isDefined)
      }
    }
  }

  it should "successfully broadcast a tx generated by gjal" in {
    val rqstString =
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "2",
         |   "method": "onlineWallet_broadcastTx",
         |   "params": [{
         |      "method": "topl_broadcastTx",
         |      "params": [{
         |        "tx": $signedPolyTx
         |      }]
         |   }]
         |}
         """.stripMargin
    val rqst = ByteString(rqstString)
    httpPOST(rqst) ~> httpOriginHeader ~> route ~> check {
      val responseString = responseAs[String].replace("\\", "")
      parse(responseString.replace("\"{", "{").replace("}\"", "}")) match {
        case Left(f) => throw f
        case Right(res: Json) =>
          assert((res \\ "error").isEmpty)
          assert((res \\ "result").head.asObject.isDefined)
      }
    }
  }

 it should "successfully disconnect from Bifrost" in {
    val disconnectRequest = ByteString(
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "2",
         |   "method": "onlineWallet_disconnectFromBifrost",
         |   "params": [{}]
         |}
         """.stripMargin)

    httpPOST(disconnectRequest) ~> httpOriginHeader ~> route ~> check {
      val responseString = responseAs[String].replace("\\", "")
      parse(responseString.replace("\"{", "{").replace("}\"", "}")) match {
        case Left(f) => throw f
        case Right(res: Json) =>
          assert((res \\ "error").isEmpty)
          assert((res \\ "result").head.asObject.isDefined)
          assert(((res \\ "result").head \\ "status").head.asString.get === "Disconnected!")
      }
    }
  }

  it should "successfully get connection status" in {
    val mnemonicPhraseRequest = ByteString(
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "2",
         |   "method": "onlineWallet_getConnection",
         |   "params": [{}]
         |}
         """.stripMargin)

    httpPOST(mnemonicPhraseRequest) ~> httpOriginHeader ~> route ~> check {
      val responseString = responseAs[String].replace("\\", "")
      parse(responseString.replace("\"\"", "\"")) match {
        case Left(f) => throw f
        case Right(res: Json) =>
          assert((res \\ "error").isEmpty)
          assert((res \\ "result").head.asObject.isDefined)
          assert(!((res \\ "result").head \\ "connectedToBifrost").head.asBoolean.get)
      }
    }
  }

  it should "successfully get network prefix" in {
    val networkTypeRequest = ByteString(
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "2",
         |   "method": "wallet_networkType",
         |   "params": [{}]
         |}
         """.stripMargin)

    httpPOST(networkTypeRequest) ~> httpOriginHeader ~> route ~> check {
      val responseString = responseAs[String].replace("\\", "")
      parse(responseString.replace("\"{", "{").replace("}\"", "}")) match {
        case Left(f) => throw f
        case Right(res: Json) =>
          assert((res \\ "error").isEmpty)
          val network = ((res \\ "result").head \\ "networkPrefix").head
          assert(network.toString() === networkPrefix.toString)
      }
    }
  }

  it should "successfully change the network" in {
    val networkTypeRequest = ByteString(
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "2",
         |   "method": "wallet_changeNetwork",
         |   "params": [{
         |      "newNetwork": "toplnet"
         |   }]
         |}
         """.stripMargin)

    httpPOST(networkTypeRequest) ~> httpOriginHeader ~> route ~> check {
      val responseString = responseAs[String].replace("\\", "")
      parse(responseString.replace("\"{", "{").replace("}\"", "}")) match {
        case Left(f) => throw f
        case Right(res: Json) =>
          assert((res \\ "error").isEmpty)
          val network = ((res \\ "result").head \\ "newNetworkPrefix").head
          assert(network.toString() === "1")
      }
    }
  }

  it should "still have keys after disconnecting from bifrost and changing network back to local" in {
    val networkTypeRequest = ByteString(
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "2",
         |   "method": "wallet_changeNetwork",
         |   "params": [{
         |      "newNetwork": "local"
         |   }]
         |}
         """.stripMargin)

    httpPOST(networkTypeRequest) ~> httpOriginHeader ~> route ~> check {
      val responseString = responseAs[String].replace("\\", "")
      parse(responseString.replace("\"{", "{").replace("}\"", "}")) match {
        case Left(f) => throw f
        case Right(res: Json) =>
          assert((res \\ "error").isEmpty)
          val network = ((res \\ "result").head \\ "newNetworkPrefix").head
          val keyfiles: Map[Address, String] = Await.result((keyManagerRef ? GetAllKeyfiles)
            .mapTo[Map[Address,String]], 10.seconds)
          keyfiles.keySet.size shouldBe 3
          assert(network.toString() === "48")
      }
    }
  }

 /* it should "successfully change the chain provider" in {
    val communicationModeRequest = ByteString(
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "2",
         |   "method": "wallet_changeCurrentChainProvider",
         |   "params": [{
         |      "chainProvider": {
         |          "type": "Http",
         |          "chainProvider": "bifrost-client@127.0.0.1:9085",
         |          "name": "Private",
         |          "network": "private",
         |          "apiKey": "test_key"
         |      }
         |   }]
         |}
         """.stripMargin)

    httpPOST(communicationModeRequest) ~> httpOriginHeader ~> route ~> check {
      val responseString = responseAs[String].replace("\\", "")
      parse(responseString.replace("\"{", "{").replace("}\"", "}")) match {
        case Left(f) => throw f
        case Right(res: Json) =>
          assert((res \\ "error").isEmpty)
          val cp = ((res \\ "result").head \\ "newChainProvider").head
          assert(cp.as[HttpChainProvider].isRight)
      }
    }
  }*/


}