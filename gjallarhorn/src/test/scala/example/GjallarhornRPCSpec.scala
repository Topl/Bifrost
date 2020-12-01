package example

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.{HttpEntity, HttpMethods, HttpRequest, MediaTypes}
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.{Http, HttpExt}
import akka.util.{ByteString, Timeout}
import crypto.{PrivateKey25519, PublicKey25519Proposition}
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import http.GjallarhornApiRoute
import akka.http.scaladsl.testkit.ScalatestRouteTest
import io.circe.Json
import io.circe.parser.parse
import keymanager.{KeyManagerRef, Keys}
import requests.{Requests, RequestsManager}
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.util.encode.Base58

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure, Success}

class GjallarhornRPCSpec extends AsyncFlatSpec
  with Matchers
  with ScalatestRouteTest
  with GjallarhornGenerators {

//  implicit val materializer: ActorMaterializer = ActorMaterializer()

  implicit val timeout: Timeout = Timeout(10.seconds)

  override def createActorSystem(): ActorSystem = ActorSystem("gjallarhornTest", config)

  val http: HttpExt = Http(system)

  val seed1: Digest32 = Blake2b256(java.util.UUID.randomUUID.toString)
  val seed2: Digest32 = Blake2b256(java.util.UUID.randomUUID.toString)
  val (sk1, pk1) = PrivateKey25519.generateKeys(seed1)
  val (sk2, pk2) = PrivateKey25519.generateKeys(seed2)

  val amount = 10

  val keyManagerRef: ActorRef = KeyManagerRef("keyManager", "keyfiles")
  val keyFileDir = "keyfiles/keyManagerTest"
  val keyManager: Keys = Keys(keyFileDir)

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
            (res \\ "error").isEmpty shouldBe true
            (res \\ "result").head.asObject.isDefined shouldBe true
        }
      }
    }

    var pubKeyAddr: String = pk1.address

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

    it should "get a successful JSON response from an unlockKeyfile request" in {
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
