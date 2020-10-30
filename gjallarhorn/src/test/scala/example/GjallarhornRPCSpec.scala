package example

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.{HttpEntity, HttpMethods, HttpRequest, MediaTypes}
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.{Http, HttpExt}
import akka.util.{ByteString, Timeout}
import crypto.PrivateKey25519Companion
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import http.GjallarhornApiRoute
import akka.http.scaladsl.testkit.ScalatestRouteTest
import io.circe.parser.parse
import keymanager.{KeyManagerRef, Keys}
import requests.Requests
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Blake2b256
import wallet.WalletManager

import scala.concurrent.duration._

class GjallarhornRPCSpec extends AsyncFlatSpec
  with Matchers
  with ScalatestRouteTest
  with GjallarhornGenerators {

  implicit val actorSystem: ActorSystem = ActorSystem()
//  implicit val materializer: ActorMaterializer = ActorMaterializer()

  implicit val timeout: Timeout = Timeout(10.seconds)
  val http: HttpExt = Http(actorSystem)

  val seed1 = Blake2b256(java.util.UUID.randomUUID.toString)
  val seed2 = Blake2b256(java.util.UUID.randomUUID.toString)
  val (sk1, pk1) = PrivateKey25519Companion.generateKeys(seed1)
  val (sk2, pk2) = PrivateKey25519Companion.generateKeys(seed2)

  val amount = 10

  val keyManagerRef: ActorRef = KeyManagerRef("keyManager", "keyfiles")
  val keyFileDir = "keyfiles/keyManagerTest"
  val keyManager = Keys(Set(), keyFileDir)
  val walletManagerRef: ActorRef = system.actorOf(Props(new WalletManager(keyManager.listOpenKeyFiles)))
  val requests: Requests = new Requests(settings)

  val route: Route = GjallarhornApiRoute(settings, keyManagerRef, walletManagerRef, requests).route

  def httpPOST(jsonRequest: ByteString): HttpRequest = {
    HttpRequest(
      HttpMethods.POST,
      uri = "/gjallarhorn/",
      entity = HttpEntity(MediaTypes.`application/json`, jsonRequest)
    ).withHeaders(RawHeader("x-api-key", "test_key"))
  }

  it should "get a successful JSON response" in {
    val createAssetRequest = ByteString(
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "2",
         |   "method": "createTransaction",
         |   "params": [{
         |     "method": "createAssetsPrototype",
         |     "params": [{
         |        "issuer": "${Base58.encode(pk1.pubKeyBytes)}",
         |        "recipient": "${Base58.encode(pk2.pubKeyBytes)}",
         |        "amount": $amount,
         |        "assetCode": "etherAssets",
         |        "fee": 0,
         |        "data": ""
         |     }]
         |   }]
         |}
         """.stripMargin)

    httpPOST(createAssetRequest) ~> route ~> check {
      val res = parse(responseAs[String]) match {case Right(re) => re; case Left(ex) => throw ex}
      (res \\ "error").isEmpty shouldBe true
      (res \\ "result").head.asObject.isDefined shouldBe true
    }
  }


}
