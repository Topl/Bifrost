package co.topl.api

import akka.http.scaladsl.model.StatusCodes.InternalServerError
import akka.util.ByteString
import co.topl.attestation.Address
import co.topl.modifier.box.AssetCode
import io.circe.Json
import io.circe.parser.parse
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scorex.crypto.hash.Blake2b256
import scorex.util.encode.Base58

import scala.util.{Failure, Success}

class UtilsRPCSpec extends AnyWordSpec with Matchers with RPCMockState {

  val seedLength: Int = 10
  val address: Address = keyRing.addresses.head

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
          case Right(re) => re;
          case Left(ex)  => throw ex
        }

        res.hcursor.downField("error").values.isEmpty shouldBe true

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
          case Right(re) => re;
          case Left(ex)  => throw ex
        }

        res.hcursor.downField("error").values.isEmpty shouldBe true

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
        val hash = res.hcursor.downField("result").get[String]("hash")

        res.hcursor.downField("error").values.isEmpty shouldBe true
        hash shouldEqual Right(Base58.encode(Blake2b256("Hello World")))
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
        val oldAssetCode: AssetCode = AssetCode(1: Byte, address, "testcode")

        val genAssetCode: AssetCode = res.hcursor.downField("result").get[AssetCode]("assetCode") match {
          case Right(re) => re;
          case Left(ex)  => throw ex
        }

        res.hcursor.downField("error").values.isEmpty shouldBe true
        oldAssetCode.toString shouldEqual genAssetCode.toString
      }
    }

    "Return the same address and network if the given address and network type are valid and matching" in {
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
        val resAddress = res.hcursor.downField("result").get[Address]("address")
        val network = res.hcursor.downField("result").get[String]("network")

        res.hcursor.downField("error").values.isEmpty shouldBe true
        network shouldEqual Right("private")
        resAddress shouldEqual Right(address)
      }
    }

    "Returns the address and the current network type, which should be private for tests, if only address is given" in {
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
        val network = res.hcursor.downField("result").get[String]("network")
        val resAddress = res.hcursor.downField("result").get[Address]("address")

        res.hcursor.downField("error").values.isEmpty shouldBe true
        network shouldEqual Right("private")
        resAddress shouldEqual Right(address)
      }
    }

    "Complain that the network type doesn't match if the received address and network type are not matching" in {
      val requestBody = ByteString(s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "1",
           |   "method": "util_checkValidAddress",
           |   "params": [{
           |      "network": "toplnet",
           |      "address": "$address"
           |   }]
           |}
      """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res: Json = parse(responseAs[String]) match { case Right(re) => re; case Left(ex) => throw ex }
        val code = res.hcursor.downField("error").get[Int]("code")
        val message = res.hcursor.downField("error").get[String]("message")

        code shouldEqual Right(InternalServerError.intValue)
        message shouldEqual Right("NetworkTypeMismatch")
      }
    }

    "Reject request if the network type doesn't exist" in {
      val requestBody = ByteString(s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "1",
           |   "method": "util_checkValidAddress",
           |   "params": [{
           |      "network": "nonexistentnetwork",
           |      "address": "$address"
           |   }]
           |}
      """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res: Json = parse(responseAs[String]) match { case Right(re) => re; case Left(ex) => throw ex }
        val code = res.hcursor.downField("error").get[Int]("code")
        val message = res.hcursor.downField("error").get[String]("message")

        code shouldEqual Right(InternalServerError.intValue)
        message shouldEqual Right("Invalid network specified")
      }
    }
  }
}
