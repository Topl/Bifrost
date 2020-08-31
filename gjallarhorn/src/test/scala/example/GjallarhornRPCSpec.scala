package example

import akka.actor.ActorSystem
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.{HttpEntity, HttpMethods, HttpRequest, MediaTypes}
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.{Http, HttpExt}
import akka.stream.ActorMaterializer
import akka.util.ByteString
import crypto.PrivateKey25519Companion
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import http.GjallarhornApiRoute
import akka.http.scaladsl.testkit.ScalatestRouteTest
import io.circe.parser.parse
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Blake2b256

class GjallarhornRPCSpec extends AsyncFlatSpec with Matchers with ScalatestRouteTest{

  implicit val actorSystem: ActorSystem = ActorSystem()
//  implicit val materializer: ActorMaterializer = ActorMaterializer()

  val http: HttpExt = Http(actorSystem)

  val seed1 = Blake2b256(java.util.UUID.randomUUID.toString)
  val seed2 = Blake2b256(java.util.UUID.randomUUID.toString)
  val (sk1, pk1) = PrivateKey25519Companion.generateKeys(seed1)
  val (sk2, pk2) = PrivateKey25519Companion.generateKeys(seed2)

  val amount = 10

  val route: Route = GjallarhornApiRoute().route

  def httpPOST(jsonRequest: ByteString): HttpRequest = {
    HttpRequest(
      HttpMethods.POST,
      uri = "http:localhost:9086/gjallarhorn/",
      entity = HttpEntity(MediaTypes.`application/json`, jsonRequest)
    ).withHeaders(RawHeader("x-api-key", "test_key"))
  }

  it should "get a successful JSON response" in {
    val createAssetRequest = ByteString(
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "2",
         |   "method": "createAssetsPrototype",
         |   "params": [{
         |     "issuer": "${Base58.encode(pk1.pubKeyBytes)}",
         |     "recipient": "${Base58.encode(pk2.pubKeyBytes)}",
         |     "amount": $amount,
         |     "assetCode": "etherAssets",
         |     "fee": 0,
         |     "data": ""
         |   }]
         |}
         """.stripMargin)

    httpPOST(createAssetRequest) ~> route ~> check {
      val res = parse(responseAs[String]).right.get
      print(res)
      (res \\ "error").isEmpty shouldBe true
      (res \\ "result").head.asObject.isDefined shouldBe true
    }
  }

}
