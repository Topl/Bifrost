package co.topl.api

import akka.http.scaladsl.server.Route
import akka.util.ByteString
import co.topl.crypto.FastCryptographicHash
import co.topl.http.api.routes.UtilsApiRoute
import io.circe.Json
import io.circe.parser.parse
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scorex.util.encode.Base58

import scala.util.{Failure, Success}

class UtilsRPCSpec extends AnyWordSpec
  with Matchers
  with RPCMockState {

  val route: Route = UtilsApiRoute(settings.restApi).route


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

      httpPOST("/utils/", requestBody) ~> route ~> check {
        val res: Json = parse(responseAs[String]) match {case Right(re) => re; case Left(ex) => throw ex}
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
           |      "length": $seedLength
           |   }]
           |}
      """.stripMargin)

      httpPOST("/utils/", requestBody) ~> route ~> check {
        val res: Json = parse(responseAs[String]) match {case Right(re) => re; case Left(ex) => throw ex}
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

      httpPOST("/utils/", requestBody) ~> route ~> check {
        val res: Json = parse(responseAs[String]) match {case Right(re) => re; case Left(ex) => throw ex}
        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").head.asObject.isDefined shouldBe true
        ((res \\ "result").head \\ "hash").head.asString.get shouldEqual Base58.encode(FastCryptographicHash("Hello World"))
      }
    }
  }
}

