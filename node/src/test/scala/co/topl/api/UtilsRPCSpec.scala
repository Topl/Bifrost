package co.topl.api

import akka.util.ByteString
import co.topl.akkahttprpc.InvalidParametersError
import co.topl.attestation.Address
import co.topl.crypto.hash.blake2b256
import co.topl.crypto.hash.implicits._
import co.topl.modifier.box.AssetCode
import co.topl.rpc.ToplRpcErrors
import co.topl.utils.StringDataTypes.{Base58Data, Latin1Data}
import co.topl.codecs.json._
import co.topl.utils.encode.Base58
import io.circe.Json
import io.circe.parser.parse
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class UtilsRPCSpec extends AnyWordSpec with Matchers with RPCMockState with EitherValues {

  val seedLength: Int = 10
  var addressCurve25519: Address = _
  var addressEd25519: Address = _

  override def beforeAll(): Unit = {
    super.beforeAll()
    addressCurve25519 = keyRingCurve25519.addresses.head
    addressEd25519 = keyRingEd25519.addresses.head
  }

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
        val res: Json = parse(responseAs[String]).value

        val seedData: Base58Data = res.hcursor.downField("result").get[Base58Data]("seed").value

        res.hcursor.downField("error").values shouldBe None

        seedData.value.length shouldEqual 32
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
        val res: Json = parse(responseAs[String]).value

        val seedData: Base58Data = res.hcursor.downField("result").get[Base58Data]("seed").value

        res.hcursor.downField("error").values shouldBe None

        seedData.value.length shouldEqual seedLength
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
        val res: Json = parse(responseAs[String]).value
        val hash = res.hcursor.downField("result").get[String]("hash").value

        hash shouldEqual Base58.encode(blake2b256.hash("Hello World".getBytes("UTF-8")).bytes)
        res.hcursor.downField("error").values shouldBe None
      }
    }

    "Generate AssetCode with given issuer and shortName" in {
      testGenerateAssetCode(addressCurve25519)
      testGenerateAssetCode(addressEd25519)

      def testGenerateAssetCode(address: Address): Unit = {
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
          val res: Json = parse(responseAs[String]).value
          val oldAssetCode: AssetCode = AssetCode(1: Byte, address, Latin1Data.unsafe("testcode"))

          val genAssetCode: AssetCode = res.hcursor.downField("result").get[AssetCode]("assetCode").value

          res.hcursor.downField("error").values shouldBe None
          oldAssetCode.toString shouldEqual genAssetCode.toString
        }
      }
    }

    "Return the same address and network if the given address and network type are valid and matching" in {
      testCheckValidAddress(addressCurve25519)
      testCheckValidAddress(addressEd25519)

      def testCheckValidAddress(address: Address): Unit = {
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
          val res: Json = parse(responseAs[String]).value
          val resAddress = res.hcursor.downField("result").get[Address]("address").value
          val network = res.hcursor.downField("result").get[String]("network").value

          res.hcursor.downField("error").values shouldBe None
          network shouldEqual "private"
          resAddress shouldEqual address
        }
      }
    }

    "Returns the address and the current network type, which should be private for tests, if only address is given" in {
      testCheckValidAddress(addressCurve25519)
      testCheckValidAddress(addressEd25519)

      def testCheckValidAddress(address: Address): Unit = {
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
          val res: Json = parse(responseAs[String]).value
          val network = res.hcursor.downField("result").get[String]("network").value
          val resAddress = res.hcursor.downField("result").get[Address]("address").value

          res.hcursor.downField("error").values shouldBe None
          network shouldEqual "private"
          resAddress shouldEqual address
        }
      }
    }

    "Complain that the network type doesn't match if the received address and network type are not matching" in {
      testCheckValidAddress(addressCurve25519)
      testCheckValidAddress(addressEd25519)

      def testCheckValidAddress(address: Address): Unit = {
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
          val res: Json = parse(responseAs[String]).value
          val code = res.hcursor.downField("error").get[Int]("code").value
          val message = res.hcursor.downField("error").get[String]("message").value

          code shouldEqual InvalidParametersError.Code
          message shouldEqual InvalidParametersError.Message
        }
      }
    }

    "Reject request if the network type doesn't exist" in {
      testCheckValidAddress(addressCurve25519)
      testCheckValidAddress(addressEd25519)

      def testCheckValidAddress(address: Address): Unit = {
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
          val res: Json = parse(responseAs[String]).value
          val code = res.hcursor.downField("error").get[Int]("code").value
          val message = res.hcursor.downField("error").get[String]("message").value

          code shouldEqual ToplRpcErrors.InvalidNetworkSpecified.code
          message shouldEqual ToplRpcErrors.InvalidNetworkSpecified.message
        }
      }
    }
  }
}
