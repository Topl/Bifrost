package co.topl.api.transaction

import akka.util.ByteString
import co.topl.api.RPCMockState
import co.topl.attestation.Address
import co.topl.utils.encode.Base58
import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax._
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ArbitTransferRPCSpec extends AnyWordSpec with Matchers with RPCMockState with EitherValues {

  var addressCurve25519: Address = _
  var addressEd25519: Address = _
  var tx = ""

  override def beforeAll(): Unit = {
    super.beforeAll()
    addressCurve25519 = keyRingCurve25519.addresses.head
    addressEd25519 = keyRingEd25519.addresses.head
  }

  def testCreateArbitTransfer(sender: Address, recipient: Address, senderPropType: String, amount: Int): Unit = {
    val requestBody = ByteString(s"""
      |{
      |   "jsonrpc": "2.0",
      |   "id": "2",
      |   "method": "topl_rawArbitTransfer",
      |   "params": [{
      |     "propositionType": "$senderPropType",
      |     "recipients": [["$recipient", "$amount"]],
      |     "sender": ["$sender"],
      |     "changeAddress": "$sender",
      |     "consolidationAddress": "$sender",
      |     "minting": "false",
      |     "fee": "1",
      |     "data": ""
      |   }]
      |}
      """.stripMargin)

    httpPOST(requestBody) ~> route ~> check {
      val res = parse(responseAs[String]).value

      val sigTx = for {
        rawTx   <- res.hcursor.downField("result").get[Json]("rawTx")
        message <- res.hcursor.downField("result").get[String]("messageToSign")
      } yield {
        val sig = senderPropType match {
          case "PublicKeyCurve25519" =>
            keyRingCurve25519.generateAttestation(addressCurve25519)(Base58.decode(message).get)
          case "PublicKeyEd25519" =>
            keyRingEd25519.generateAttestation(addressEd25519)(Base58.decode(message).get)
        }
        val signatures: Json = Map(
          "signatures" -> sig.asJson
        ).asJson
        rawTx.deepMerge(signatures)
      }

      tx = sigTx.value.toString

      (res \\ "error").isEmpty shouldBe true
      (res \\ "result").head.asObject.isDefined shouldBe true
    }
  }

  def testBroadcastTx(): Unit = {
    val requestBody = ByteString(s"""
      |{
      |   "jsonrpc": "2.0",
      |   "id": "2",
      |   "method": "topl_broadcastTx",
      |   "params": [{
      |     "tx": $tx
      |   }]
      |}
      |""".stripMargin)

    httpPOST(requestBody) ~> route ~> check {
      val res = parse(responseAs[String]).value
      (res \\ "error").isEmpty shouldBe true
      (res \\ "result").head.asObject.isDefined shouldBe true
    }
  }

  "ArbitTransfer RPC" should {
    "Create and sign new arbit transfer raw transaction from non empty Curve25519 address to itself" in {
      testCreateArbitTransfer(addressCurve25519, addressCurve25519, propTypeCurve25519, 3)
    }

    "Broadcast signed ArbitTransfer transaction from an Curve25519 address to itself" in {
      testBroadcastTx()
    }

    "Create and sign new arbit transfer raw transaction from non empty Curve25519 address to an empty Ed25519" +
    " address" in {
      testCreateArbitTransfer(addressCurve25519, addressEd25519, propTypeCurve25519, 3)
    }

    "Broadcast signed ArbitTransfer transaction from an Curve25519 address to an Ed25519 address" in {
      testBroadcastTx()
    }

    "Create and sign new arbit transfer raw transaction from non empty Ed25519 address to itself" in {
      testCreateArbitTransfer(addressEd25519, addressEd25519, propTypeEd25519, 0)
    }

    "Broadcast signed ArbitTransfer transaction from an Ed25519 address to itself" in {
      testBroadcastTx()
    }
  }
}
