package bifrost.api

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.model.{HttpEntity, HttpMethods, HttpRequest, MediaTypes}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.pattern.ask
import akka.util.{ByteString, Timeout}
import bifrost.api.http.{NodeViewApiRouteRPC, UtilsApiRouteRPC}
import bifrost.history.BifrostHistory
import bifrost.mempool.BifrostMemPool
import bifrost.scorexMod.GenericNodeViewHolder.{CurrentView, GetCurrentView}
import bifrost.state.BifrostState
import bifrost.wallet.BWallet
import bifrost.{BifrostGenerators, BifrostNodeViewHolder}
import io.circe.Json
import io.circe.parser.parse
import org.scalatest.{Matchers, WordSpec}
import bifrost.block.Block
import bifrost.blocks.BifrostBlock
import bifrost.crypto.hash.FastCryptographicHash
import bifrost.transaction.BifrostTransaction
import bifrost.transaction.box.ArbitBox
import bifrost.transaction.box.proposition.PublicKey25519Proposition
import bifrost.transaction.proof.Signature25519
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.Curve25519
import scala.util.{Failure, Success, Try}
import scala.concurrent.Await
import scala.concurrent.duration._


class UtilsRPCSpec extends WordSpec
  with Matchers
  with ScalatestRouteTest
  with BifrostGenerators {

  val route = UtilsApiRouteRPC(settings).route

  def httpPOST(jsonRequest: ByteString): HttpRequest = {
    HttpRequest(
      HttpMethods.POST,
      uri = "/utilsRPC/",
      entity = HttpEntity(MediaTypes.`application/json`, jsonRequest)
    )
  }

  val seedLength: Int = 10

  "Utils RPC" should {

    "Generate random seed" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "30",
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
           |   "id": "30",
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
           |   "id": "30",
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

