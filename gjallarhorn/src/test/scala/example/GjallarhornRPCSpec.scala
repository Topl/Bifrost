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
import http.GjallarhornApiRoute
import akka.http.scaladsl.testkit.ScalatestRouteTest
import crypto.AddressEncoder.NetworkPrefix
import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax.EncoderOps
import keymanager.{KeyManagerRef, Keys}
import requests.{Requests, RequestsManager}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure, Success}

class GjallarhornRPCSpec extends AsyncFlatSpec
  with Matchers
  with ScalatestRouteTest
  with GjallarhornGenerators {

//  implicit val materializer: ActorMaterializer = ActorMaterializer()

  implicit val timeout: Timeout = Timeout(10.seconds)
  implicit val networkPrefix: NetworkPrefix = 1.toByte

  override def createActorSystem(): ActorSystem = ActorSystem("gjallarhornTest", config)

  val http: HttpExt = Http(system)
  val amount = 10

  val keyManagerRef: ActorRef = KeyManagerRef("keyManager", "keyfiles")
  val keyFileDir = "keyfiles/keyManagerTest"
  val keyManager: Keys[PrivateKeyCurve25519, KeyfileCurve25519] = Keys(keyFileDir, KeyfileCurve25519)

  val pk1: Address = keyManager.generateKeyFile("password1") match {
    case Success(address) => address
    case Failure(ex) => throw ex
  }
  val pk2: Address = keyManager.generateKeyFile("password2") match {
    case Success(address) => address
    case Failure(ex) => throw ex
  }

  val bifrostActor: ActorRef = Await.result(system.actorSelection(
    s"akka.tcp://${settings.chainProvider}/user/walletConnectionHandler").resolveOne(), 10.seconds)
  val requestsManagerRef: ActorRef = system.actorOf(Props(new RequestsManager(bifrostActor)), name = "RequestsManager")
  val requests: Requests = new Requests(settings, requestsManagerRef)
  val route: Route = GjallarhornApiRoute(settings, keyManagerRef, requestsManagerRef, requests).route


  def httpPOST(jsonRequest: ByteString): HttpRequest = {
    HttpRequest(
      HttpMethods.POST,
      uri = "/gjallarhorn/",
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
         |   "method": "createTransaction",
         |   "params": [{
         |     "method": "createAssetsPrototype",
         |     "params": [{
         |        "issuer": "${pk1.toString}",
         |        "recipient": "${pk2.toString}",
         |        "amount": $amount,
         |        "assetCode": "etherAssets",
         |        "fee": 0,
         |        "data": ""
         |     }]
         |   }]
         |}
       """.stripMargin)

    httpPOST(createAssetRequest) ~> route ~> check {
      val responseString = responseAs[String].replace("\\", "")
      parse(responseString.replace("\"{", "{").replace("}\"", "}")) match {
        case Left(f) => throw f
        case Right(res: Json) =>
          println(res)
          prototypeTx = (res \\ "formattedTx").head
          msgToSign = (res \\ "messageToSign").head.asString.get
          (res \\ "error").isEmpty shouldBe true
          (res \\ "result").head.asObject.isDefined shouldBe true
      }
    }
  }

  //TODO: make sure it works after changing keys format.
/*  it should "successfully sign a transaction" in {
    println(prototypeTx)
    println(msgToSign)
    val signTxRequest = ByteString(
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "2",
         |   "method": "signTx",
         |   "params": [{
         |      "signingKeys": ["${pk1.toString}"],
         |      "protoTx": $prototypeTx,
         |      "messageToSign": "$msgToSign"
         |   }]
         |}
         """.stripMargin)

    httpPOST(signTxRequest) ~> route ~> check {
      val responseString = responseAs[String].replace("\\", "")
      parse(responseString.replace("\"{", "{").replace("}\"", "}")) match {
        case Left(f) => throw f
        case Right(res: Json) =>
          println(res)
          (res \\ "error").isEmpty shouldBe true
          (res \\ "result").head.asObject.isDefined shouldBe true
      }
    }
  }*/

  var pubKeyAddr: String = pk1.toString

  it should "successfully generate a keyfile" in {
    val generateKeyfileRequest = ByteString(
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "2",
         |   "method": "generateKeyfile",
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
         |   "method": "importKeyfile",
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
         |   "method": "lockKeyfile",
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
         |   "method": "unlockKeyfile",
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

  //ToDo: listOpenKeyFiles isn't returning correct form of the keyfiles.
  /*it should "successfully get open keyfiles" in {
    val openKeyfilesRequest = ByteString(
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "2",
         |   "method": "listOpenKeyfiles",
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
          println(openKeys)
          println(pubKeyAddr)
          openKeys.contains(s"$pubKeyAddr") shouldBe true
      }
    }
  }*/

  it should "get a successful JSON response from balance request" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "1",
           |   "method": "balances",
           |   "params": [{
           |      "method": "balances",
           |      "params": [{
           |            "publicKeys": ["$pubKeyAddr"]
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

}
