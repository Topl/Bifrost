package co.topl.api.transaction

import akka.util.ByteString
import co.topl.api.RPCMockState
import co.topl.attestation.Address
import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scorex.util.encode.Base58

class PolyTransferRPCSpec extends AnyWordSpec
  with Matchers
  with RPCMockState {

  val address: Address = keyRing.addresses.head
  var tx = ""

  "PolyTransfer RPC" should {
    "Create new poly transfer raw transaction" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "2",
           |   "method": "topl_rawPolyTransfer",
           |   "params": [{
           |     "propositionType": "PublicKeyCurve25519",
           |     "recipients": [["$address", "1"]],
           |     "sender": ["$address"],
           |     "changeAddress": "$address",
           |     "fee": "1",
           |     "data": ""
           |   }]
           |}
        """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]) match {
          case Right(re) => re;
          case Left(ex) => throw ex
        }

        val sigTx = for {
          rawTx <- res.hcursor.downField("result").get[Json]("rawTx")
          message <- res.hcursor.downField("result").get[String]("messageToSign")
        } yield {
          val sig = keyRing.generateAttestation(address)(Base58.decode(message).get)
          val signatures: Json = Map(
            "signatures" -> sig.asJson
          ).asJson
          rawTx.deepMerge(signatures)
        }

        tx = sigTx.right.get.toString

        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").head.asObject.isDefined shouldBe true
      }
    }

    "Broadcast signed PolyTransfer transaction" in {
      val requestBody = ByteString(
        s"""
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
        val res = parse(responseAs[String]) match { case Right(re) => re; case Left(ex) => throw ex }
        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").head.asObject.isDefined shouldBe true
      }
    }
  }
}
