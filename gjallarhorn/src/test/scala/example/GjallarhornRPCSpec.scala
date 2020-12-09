package example

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.{HttpEntity, HttpMethods, HttpRequest, MediaTypes}
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.{Http, HttpExt}
import akka.util.{ByteString, Timeout}
import crypto.{Address, KeyfileCurve25519, PrivateKeyCurve25519}
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import http.{GjallarhornApiRoute, HttpService}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import crypto.AddressEncoder.NetworkPrefix
import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax.EncoderOps
import keymanager.{KeyManagerRef, Keys}
import requests.{ApiRoute, Requests, RequestsManager}
import wallet.WalletManager
import wallet.WalletManager.{GjallarhornStarted, YourKeys}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure, Success}

/**
  * Must be running bifrost with --local and --seed test
  * ex: "run --local --seed test -f"
  */
class GjallarhornRPCSpec extends AsyncFlatSpec
  with Matchers
  with ScalatestRouteTest
  with GjallarhornGenerators {

//  implicit val materializer: ActorMaterializer = ActorMaterializer()

  implicit val timeout: Timeout = Timeout(10.seconds)
  /**
    * Make sure running bifrost in local network!
    */
  implicit val networkPrefix: NetworkPrefix = 48.toByte

  override def createActorSystem(): ActorSystem = ActorSystem("gjallarhornTest", config)

  val http: HttpExt = Http(system)
  val amount = 10

  val keyFileDir = "keyfiles/keyManagerTest"
  val keyManager: Keys[PrivateKeyCurve25519, KeyfileCurve25519] =
    Keys[PrivateKeyCurve25519, KeyfileCurve25519](keyFileDir, KeyfileCurve25519)
  val keyManagerRef: ActorRef = KeyManagerRef("keyManager", keyManager)

  val privateKeys: Set[PrivateKeyCurve25519] = keyManager.generateNewKeyPairs(2, Some("test")) match {
    case Success(secrets) => secrets
    case Failure(ex) => throw new Error (s"Unable to generate new keys: $ex")
  }

  val addresses: Set[Address] = privateKeys.map(sk => sk.publicImage.address)

  val (pk1, sk1) = (addresses.head, privateKeys.head)
  val (pk2, sk2) = (addresses.tail.head, privateKeys.tail.head)

  val bifrostActor: ActorRef = Await.result(system.actorSelection(
    s"akka.tcp://${settings.application.chainProvider}/user/walletConnectionHandler").resolveOne(), 10.seconds)
  val walletManagerRef: ActorRef = system.actorOf(
    Props(new WalletManager(bifrostActor)), name = "WalletManager")
  walletManagerRef ! GjallarhornStarted
  walletManagerRef ! YourKeys(addresses)
  val requestsManagerRef: ActorRef = system.actorOf(Props(new RequestsManager(bifrostActor)), name = "RequestsManager")
  val requests: Requests = new Requests(settings.application, requestsManagerRef)
  val apiRoute: ApiRoute = GjallarhornApiRoute(settings, keyManagerRef, requestsManagerRef, walletManagerRef, requests)
  val route: Route = HttpService(Seq(apiRoute), settings.rpcApi).compositeRoute


  def httpPOST(jsonRequest: ByteString): HttpRequest = {
    HttpRequest(
      HttpMethods.POST,
      uri = "/",
      entity = HttpEntity(MediaTypes.`application/json`, jsonRequest)
    ).withHeaders(RawHeader("x-api-key", "test_key"))
  }

  var prototypeTx: Json = Map("txType" -> "AssetCreation").asJson
  var msgToSign = ""

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
         |        "sender": ["$pk2"],
         |        "recipient": [["$pk2", $amount]],
         |        "changeAddress": "$pk2",
         |        "issuer": "$pk2",
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

  it should "successfully get open keyfiles" in {
    val openKeyfilesRequest = ByteString(
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "2",
         |   "method": "wallet_listOpenKeyfiles",
         |   "params": [{}]
         |}
         """.stripMargin)

    httpPOST(openKeyfilesRequest) ~> route ~> check {
      val responseString = responseAs[String].replace("\\", "")
      parse(responseString.replace("\"{", "{").replace("}\"", "}")) match {
        case Left(f) => throw f
        case Right(res: Json) =>
          (res \\ "error").isEmpty shouldBe true
          val openKeys: Set[String] = (res \\ "result").head.asArray.get.map(k => k.asString.get).toSet
          openKeys.contains(pk1.toString) shouldBe true
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
         |        "sender": ["$pk2"],
         |        "recipient": [["$pk1", $amount]],
         |        "changeAddress": "$pk2",
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

  var pubKeyAddr: String = pk1.toString

  it should "successfully generate a keyfile" in {
    val generateKeyfileRequest = ByteString(
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "2",
         |   "method": "wallet_generateKeyfile",
         |   "params": [{
         |      "password": "foo"
         |   }]
         |}
         """.stripMargin)

    httpPOST(generateKeyfileRequest) ~> route ~> check {
      parse(responseAs[String]) match {
        case Left(f) => throw f
        case Right(res: Json) =>
          val result: Json = (res \\ "result").head
          pubKeyAddr = (result \\ "address").head.asString.get
          (res \\ "error").isEmpty shouldBe true
          result.asObject.isDefined shouldBe true
      }
    }
  }

  val seedPhrase = "stand earth guess employ goose aisle great next embark weapon wonder aisle monitor surface omit guilt model rule"

  it should "successfully import a keyfile through mnemonic phrase" in {
    val importKeyfileRequest = ByteString(
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "2",
         |   "method": "wallet_importKeyfile",
         |   "params": [{
         |      "password": "password",
         |      "seedPhrase": "$seedPhrase",
         |      "seedPhraseLang": "en"
         |   }]
         |}
         """.stripMargin)

    httpPOST(importKeyfileRequest) ~> route ~> check {
      parse(responseAs[String]) match {
        case Left(f) => throw f
        case Right(res: Json) =>
          val result: Json = (res \\ "result").head
          (res \\ "error").isEmpty shouldBe true
          result.asObject.isDefined shouldBe true
      }
    }
  }

  it should "successfully lock a Keyfile" in {
    val lockKeyRequest = ByteString(
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "2",
         |   "method": "wallet_lockKeyfile",
         |   "params": [{
         |      "publicKey": "$pubKeyAddr",
         |      "password": "foo"
         |   }]
         |}
           """.stripMargin)

    httpPOST(lockKeyRequest) ~> route ~> check {
      val responseString = responseAs[String].replace("\\", "")
      parse(responseString.replace("\"{", "{").replace("}\"", "}")) match {
        case Left(f) => throw f
        case Right(res: Json) =>
          (res \\ "error").isEmpty shouldBe true
          (res \\ "result").head.asObject.isDefined shouldBe true
      }
    }
  }

  it should "successfully unlock a Keyfile" in {
    val unlockKeyRequest = ByteString(
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "2",
         |   "method": "wallet_unlockKeyfile",
         |   "params": [{
         |      "publicKey": "$pubKeyAddr",
         |      "password": "foo"
         |   }]
         |}
         """.stripMargin)

    httpPOST(unlockKeyRequest) ~> route ~> check {
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
         |   "params": [{
         |      "method": "topl_balances",
         |      "params": [{
         |            "addresses": ["$pk1", "$pk2"]
         |       }]
         |   }]
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

  it should "successfully get mnemonic phrase" in {
    val mnemonicPhraseRequest = ByteString(
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "2",
         |   "method": "wallet_generateMnemonic",
         |   "params": [{
         |      "language": "en"
         |   }]
         |}
         """.stripMargin)

    httpPOST(mnemonicPhraseRequest) ~> route ~> check {
      val responseString = responseAs[String].replace("\\", "")
      parse(responseString.replace("\"{", "{").replace("}\"", "}")) match {
        case Left(f) => throw f
        case Right(res: Json) =>
          (res \\ "error").isEmpty shouldBe true
          val phrase = ((res \\ "result").head \\ "mnemonicPhrase").head
          assert(phrase != null)
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
        case Right(res: Json) => {
          (res \\ "error").isEmpty shouldBe true
          val phrase = (res \\ "result").head
          println (res)
          assert (phrase != null)
        }
      }
    }
  }

}
