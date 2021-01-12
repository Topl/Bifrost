package co.topl.api

import akka.util.ByteString
import co.topl.attestation.Address
import co.topl.nodeView.state.box.AssetCode
import co.topl.nodeView.state.box.AssetCode.parseBytes
import io.circe.Json
import io.circe.parser.parse
import kamon.trace.Span.Empty
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scorex.crypto.hash.Blake2b256
import scorex.util.encode.Base58

import scala.util.{Failure, Success}

class UtilsRPCSpec extends AnyWordSpec with Matchers with RPCMockState {

  val seedLength: Int = 10
  val address: Address = keyRing.addresses.last

  "Utils RPC" should {
    "Generate random seed" in {
      val requestBody = ByteString(s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "1",
           |   "method": "util_seed",
           |   "params": [{}]
           |}
        """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res: Json = parse(responseAs[String]) match { case Right(re) => re; case Left(ex) => throw ex }

        val seedString: String = res.hcursor.downField("result").get[String]("seed") match {
          case Right(re) => re; case Left(ex) => throw ex
        }

        res.hcursor.downField("error").values.isEmpty shouldBe true
        res.hcursor.downField("result").succeeded shouldBe true

        Base58.decode(seedString) match {
          case Success(seed) => seed.length shouldEqual 32
          case Failure(_)    => fail("Could not Base 58 decode seed output")
        }
      }
    }

    "Generate random of given length" in {
      val requestBody = ByteString(s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "1",
           |   "method": "util_seedOfLength",
           |   "params": [{
           |      "length": $seedLength
           |   }]
           |}
      """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res: Json = parse(responseAs[String]) match { case Right(re) => re; case Left(ex) => throw ex }

        val seedString: String = res.hcursor.downField("result").get[String]("seed") match {
          case Right(re) => re; case Left(ex) => throw ex
        }

        res.hcursor.downField("error").values.isEmpty shouldBe true
        res.hcursor.downField("result").succeeded shouldBe true

        Base58.decode(seedString) match {
          case Success(seed) => seed.length shouldEqual seedLength
          case Failure(_)    => fail("Could not Base 58 decode seed output")
        }
      }
    }

    "Return blake2b hash of given message" in {
      val requestBody = ByteString(s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "1",
           |   "method": "util_hashBlake2b256",
           |   "params": [{
           |      "message": "Hello World"
           |   }]
           |}
      """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res: Json = parse(responseAs[String]) match { case Right(re) => re; case Left(ex) => throw ex }

        val hash: String = res.hcursor.downField("result").get[String]("hash") match {
          case Right(re) => re; case Left(ex) => throw ex
        }

        res.hcursor.downField("error").values.isEmpty shouldBe true
        res.hcursor.downField("result").succeeded shouldBe true
        hash shouldEqual Base58.encode(Blake2b256("Hello World"))
      }
    }

    "Generate AssetCode with given issuer and shortName" in {
      val requestBody = ByteString(s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "1",
           |   "method": "util_generateAssetCode",
           |   "params": [{
           |      "version": 1,
           |      "issuer": "$address",
           |      "shortName": "testcode"
           |   }]
           |}
      """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res: Json = parse(responseAs[String]) match { case Right(re) => re; case Left(ex) => throw ex }

        val assetCode: AssetCode = res.hcursor.downField("result").get[AssetCode]("assetCode") match {
          case Right(re) => re; case Left(ex) => throw ex
        }

        res.hcursor.downField("error").values.isEmpty shouldBe true
        res.hcursor.downField("result").succeeded shouldBe true
        assetCode.shortName shouldEqual "testcode"
        assetCode.issuer == address shouldBe true
      }
    }

    "Returns the address and network if valid address and network are given" in {
      val requestBody = ByteString(s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "1",
           |   "method": "util_checkValidAddress",
           |   "params": [{
           |      "network": "private",
           |      "address": "$address"
           |   }]
           |}
      """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res: Json = parse(responseAs[String]) match { case Right(re) => re; case Left(ex) => throw ex }

        val resAddress: Address = res.hcursor.downField("result").get[Address]("address") match {
          case Right(re) => re; case Left(ex) => throw ex
        }

        val network: String = res.hcursor.downField("result").get[String]("network") match {
          case Right(re) => re; case Left(ex) => throw ex
        }

        res.hcursor.downField("error").values.isEmpty shouldBe true
        res.hcursor.downField("result").succeeded shouldBe true
        network == "private" shouldBe true
        resAddress == address shouldBe true
      }
    }

    "Returns the address and network if only the valid address is given" in {
      val requestBody = ByteString(s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "1",
           |   "method": "util_checkValidAddress",
           |   "params": [{
           |      "address": "$address"
           |   }]
           |}
      """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res: Json = parse(responseAs[String]) match { case Right(re) => re; case Left(ex) => throw ex }

        val resAddress: Address = res.hcursor.downField("result").get[Address]("address") match {
          case Right(re) => re; case Left(ex) => throw ex
        }

        val network: String = res.hcursor.downField("result").get[String]("network") match {
          case Right(re) => re; case Left(ex) => throw ex
        }

        res.hcursor.downField("error").values.isEmpty shouldBe true
        res.hcursor.downField("result").succeeded shouldBe true
        network == "private" shouldBe true
        resAddress == address shouldBe true
      }
    }
  }
}
