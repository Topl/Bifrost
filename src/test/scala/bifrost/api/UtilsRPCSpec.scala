package bifrost.api

import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.{HttpEntity, HttpMethods, HttpRequest, MediaTypes}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.util.ByteString
import bifrost.BifrostGenerators
import bifrost.crypto.FastCryptographicHash
import bifrost.http.api.routes.UtilsApiRoute
import io.circe.parser.parse
import scorex.util.encode.Base58

import scala.util.{Failure, Success}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec


class UtilsRPCSpec extends AnyWordSpec
  with Matchers
  with ScalatestRouteTest
  with BifrostGenerators {

  val route = UtilsApiRoute(settings.restApi).route

  def httpPOST(jsonRequest: ByteString): HttpRequest = {
    HttpRequest(
      HttpMethods.POST,
      uri = "/utils/",
      entity = HttpEntity(MediaTypes.`application/json`, jsonRequest)
    ).withHeaders(RawHeader("x-api-key", "test_key"))
  }

  val seedLength: Int = 10

  "Utils RPC" should {
    "Generate random seed" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "1",
           |   "method": "seed",
           |   "params": [{}]
           |}
        """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").head.asObject.isDefined shouldBe true
        Base58.decode(((res \\ "result").head \\ "seed").head.asString.get) match {
          case Success(seed) => seed.length shouldEqual 32
          case Failure(_) => fail("Could not Base 58 decode seed output")
        }
      }
    }

    "Generate random of given length" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "1",
           |   "method": "seedOfLength",
           |   "params": [{
           |      "length": ${seedLength}
           |   }]
           |}
      """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").head.asObject.isDefined shouldBe true
        Base58.decode(((res \\ "result").head \\ "seed").head.asString.get) match {
          case Success(seed) => seed.length shouldEqual seedLength
          case Failure(_) => fail("Could not Base 58 decode seed output")
        }
      }
    }

    "Return blake2b hash of given message" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "1",
           |   "method": "hashBlake2b",
           |   "params": [{
           |      "message": "Hello World"
           |   }]
           |}
      """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").head.asObject.isDefined shouldBe true
        ((res \\ "result").head \\ "hash").head.asString.get shouldEqual Base58.encode(FastCryptographicHash("Hello World"))
      }
    }
  }
}

