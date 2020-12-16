package example

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.{HttpEntity, HttpMethods, HttpRequest, MediaTypes}
import akka.http.scaladsl.server.Route
import akka.util.{ByteString, Timeout}
import crypto.Address
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import http.{GjallarhornBifrostApiRoute, GjallarhornOnlyApiRoute, HttpService}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import crypto.AddressEncoder.NetworkPrefix
import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax.EncoderOps
import keymanager.KeyManager.GenerateKeyFile
import keymanager.KeyManagerRef
import requests.{ApiRoute, Requests, RequestsManager}
import wallet.WalletManager
import wallet.WalletManager.{GjallarhornStarted, KeyManagerReady}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

/**
  * Must be running bifrost with --local and --seed test
  * ex: "run --local --seed test -f"
  */
class GjallarhornRPCSpec extends AsyncFlatSpec
  with Matchers
  with GjallarhornGenerators
  with ScalatestRouteTest {

//  implicit val materializer: ActorMaterializer = ActorMaterializer()

  implicit val timeout: Timeout = Timeout(10.seconds)
  /**
    * Make sure running bifrost in local network!
    */
  implicit val networkPrefix: NetworkPrefix = 48.toByte

  override def createActorSystem(): ActorSystem = ActorSystem("gjallarhornTest", config)

  val keyFileDir = "keyfiles/keyManagerTest"
  val keyManagerRef: ActorRef = KeyManagerRef("keyManager", keyFileDir)

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

  val amount = 10

  val bifrostActor: ActorRef = Await.result(system.actorSelection(
    s"akka.tcp://${settings.application.chainProvider}/user/walletConnectionHandler").resolveOne(), 10.seconds)
  val walletManagerRef: ActorRef = system.actorOf(
    Props(new WalletManager(bifrostActor)), name = "WalletManager")
  walletManagerRef ! GjallarhornStarted
  walletManagerRef ! KeyManagerReady(keyManagerRef)
  val requestsManagerRef: ActorRef = system.actorOf(Props(new RequestsManager(bifrostActor)), name = "RequestsManager")
  val requests: Requests = new Requests(settings.application, requestsManagerRef, keyManagerRef)
  val bifrostApiRoute: ApiRoute = GjallarhornBifrostApiRoute(settings, keyManagerRef, requestsManagerRef,
    walletManagerRef, requests)
  val gjalOnlyApiRoute: ApiRoute = GjallarhornOnlyApiRoute(settings, keyManagerRef)
  val route: Route = HttpService(Seq(bifrostApiRoute, gjalOnlyApiRoute), settings.rpcApi).compositeRoute


  def httpPOST(jsonRequest: ByteString): HttpRequest = {
    HttpRequest(
      HttpMethods.POST,
      uri = "/",
      entity = HttpEntity(MediaTypes.`application/json`, jsonRequest)
    ).withHeaders(RawHeader("x-api-key", "test_key"))
  }

  var prototypeTx: Json = Map("txType" -> "AssetCreation").asJson
  var msgToSign = ""

  it should "get a successful JSON response from createTx request" in {
    val createAssetRequest = ByteString(
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "2",
         |   "method": "wallet_createTransaction",
         |   "params": [{
         |     "method": "topl_rawAssetTransfer",
         |     "params": [{
         |        "propositionType": "PublicKeyCurve25519",
         |        "sender": ["$pk1"],
         |        "recipient": [["$pk1", $amount]],
         |        "changeAddress": "$pk1",
         |        "issuer": "$pk1",
         |        "assetCode": "test",
         |        "minting": true,
         |        "fee": 1,
         |        "data": "",
         |        "online": false
         |     }]
         |   }]
         |}
       """.stripMargin)

    httpPOST(createAssetRequest) ~> route ~> check {
      val responseString = responseAs[String].replace("\\", "")
      parse(responseString.replace("\"{", "{").replace("}\"", "}")) match {
        case Left(f) => throw f
        case Right(res: Json) =>
          prototypeTx = (res \\ "rawTx").head
          msgToSign = (res \\ "messageToSign").head.asString.get
          (res \\ "error").isEmpty shouldBe true
          (res \\ "result").head.asObject.isDefined shouldBe true
      }
    }
  }

  it should "successfully create raw poly tx" in {
    val createPolyRequest = ByteString(
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "2",
         |   "method": "wallet_createTransaction",
         |   "params": [{
         |     "method": "topl_rawPolyTransfer",
         |     "params": [{
         |        "propositionType": "PublicKeyCurve25519",
         |        "sender": ["$pk2"],
         |        "recipient": [["$pk1", $amount]],
         |        "changeAddress": "$pk2",
         |        "fee": 1,
         |        "data": "",
         |        "online": false
         |     }]
         |   }]
         |}
       """.stripMargin)

    httpPOST(createPolyRequest) ~> route ~> check {
      val responseString = responseAs[String].replace("\\", "")
      parse(responseString.replace("\"{", "{").replace("}\"", "}")) match {
        case Left(f) => throw f
        case Right(res: Json) =>
          (res \\ "error").isEmpty shouldBe true
          (res \\ "result").head.asObject.isDefined shouldBe true
      }
    }
  }

  it should "successfully send online arbit tx" in {
    val createPolyRequest = ByteString(
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "2",
         |   "method": "wallet_createTransaction",
         |   "params": [{
         |     "method": "topl_rawPolyTransfer",
         |     "params": [{
         |        "propositionType": "PublicKeyCurve25519",
         |        "sender": ["$pk1"],
         |        "recipient": [["$pk2", $amount]],
         |        "changeAddress": "$pk1",
         |        "fee": 1,
         |        "data": "",
         |        "online": true
         |     }]
         |   }]
         |}
       """.stripMargin)

    httpPOST(createPolyRequest) ~> route ~> check {
      val responseString = responseAs[String].replace("\\", "")
      parse(responseString.replace("\"{", "{").replace("}\"", "}")) match {
        case Left(f) => throw f
        case Right(res: Json) =>
          (res \\ "error").isEmpty shouldBe true
          (res \\ "result").head.asObject.isDefined shouldBe true
      }
    }
  }

  it should "successfully sign a transaction" in {
    val signTxRequest = ByteString(
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "2",
         |   "method": "wallet_signTx",
         |   "params": [{
         |      "signingKeys": ["${pk1.toString}"],
         |      "rawTx": $prototypeTx,
         |      "messageToSign": "$msgToSign"
         |   }]
         |}
         """.stripMargin)

    httpPOST(signTxRequest) ~> route ~> check {
      val responseString = responseAs[String].replace("\\", "")
      parse(responseString.replace("\"{", "{").replace("}\"", "}")) match {
        case Left(f) => throw f
        case Right(res: Json) =>
          (res \\ "error").isEmpty shouldBe true
          (res \\ "result").head.asObject.isDefined shouldBe true
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

      httpPOST(requestBody) ~> route ~> check {
        val responseString = responseAs[String].replace("\\", "")
        parse(responseString.replace("\"{", "{").replace("}\"", "}")) match {
          case Left(f) => throw f
          case Right(res: Json) =>
            (res \\ "error").isEmpty shouldBe true
            (res \\ "result").head.asObject.isDefined shouldBe true
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

    httpPOST(mnemonicPhraseRequest) ~> route ~> check {
      val responseString = responseAs[String].replace("\\", "")
      parse(responseString.replace("\"\"", "\"")) match {
        case Left(f) => throw f
        case Right(res: Json) =>
          (res \\ "error").isEmpty shouldBe true
          val phrase = (res \\ "result").head
          assert (phrase != null)
      }
    }
  }

  it should "successfully get connection status" in {
    val mnemonicPhraseRequest = ByteString(
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "2",
         |   "method": "wallet_connectedToBifrost",
         |   "params": [{}]
         |}
         """.stripMargin)

    httpPOST(mnemonicPhraseRequest) ~> route ~> check {
      val responseString = responseAs[String].replace("\\", "")
      parse(responseString.replace("\"\"", "\"")) match {
        case Left(f) => throw f
        case Right(res: Json) =>
          (res \\ "error").isEmpty shouldBe true
          (res \\ "result").head.asObject.isDefined shouldBe true
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

    httpPOST(networkTypeRequest) ~> route ~> check {
      val responseString = responseAs[String].replace("\\", "")
      parse(responseString.replace("\"{", "{").replace("}\"", "}")) match {
        case Left(f) => throw f
        case Right(res: Json) =>
          (res \\ "error").isEmpty shouldBe true
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

    httpPOST(networkTypeRequest) ~> route ~> check {
      val responseString = responseAs[String].replace("\\", "")
      parse(responseString.replace("\"{", "{").replace("}\"", "}")) match {
        case Left(f) => throw f
        case Right(res: Json) =>
          println(res)
          (res \\ "error").isEmpty shouldBe true
          val network = ((res \\ "result").head \\ "newNetworkPrefix").head
          assert(network.toString() === "1")
      }
    }
  }

}
