package co.topl.api.transaction

import akka.util.ByteString
import co.topl.api.RPCMockState
import co.topl.attestation.Address
import co.topl.modifier.box.AssetCode
import co.topl.utils.encode.Base58
import co.topl.utils.IdiomaticScalaTransition.implicits._
import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax._
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

trait TransferRPCTestMethods extends AnyWordSpec with Matchers with RPCMockState with EitherValues {

  def testBroadcastTx(tx: String): Unit = {
    val requestBody = ByteString(s"""
                                    |{
                                    | "jsonrpc": "2.0",
                                    | "id": "2",
                                    | "method": "topl_broadcastTx",
                                    | "params": [{
                                    |   "tx": $tx
                                    | }]
                                    |}
                                    |""".stripMargin)

    httpPOST(requestBody) ~> route ~> check {
      val res = parse(responseAs[String]) match { case Right(re) => re; case Left(ex) => throw ex }

      (res \\ "error").isEmpty shouldBe true
      (res \\ "result").head.asObject.isDefined shouldBe true
    }
  }

  def testCreateSignPolyTransfer(sender: Address, recipient: Address, senderPropType: String, amount: Int): String = {
    val requestBody = ByteString(s"""
                                    |{
                                    | "jsonrpc": "2.0",
                                    | "id": "2",
                                    | "method": "topl_rawPolyTransfer",
                                    | "params": [{
                                    |   "propositionType": "$senderPropType",
                                    |   "recipients": [["$recipient", "$amount"]],
                                    |   "sender": ["$sender"],
                                    |   "changeAddress": "$sender",
                                    |   "consolidationAddress": "$sender",
                                    |   "minting": "false",
                                    |   "fee": "1",
                                    |   "data": ""
                                    | }]
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
            keyRingCurve25519.generateAttestation(sender)(Base58.decode(message).getOrThrow())
          case "PublicKeyEd25519" => keyRingEd25519.generateAttestation(sender)(Base58.decode(message).getOrThrow())
        }
        val signatures: Json = Map(
          "signatures" -> sig.asJson
        ).asJson
        rawTx.deepMerge(signatures)
      }

      (res \\ "error").isEmpty shouldBe true
      (res \\ "result").head.asObject.isDefined shouldBe true

      sigTx.value.toString
    }
  }

  def testCreateSignArbitTransfer(sender: Address, recipient: Address, senderPropType: String, amount: Int): String = {
    val requestBody = ByteString(s"""
                                    |{
                                    | "jsonrpc": "2.0",
                                    | "id": "2",
                                    | "method": "topl_rawArbitTransfer",
                                    | "params": [{
                                    |   "propositionType": "$senderPropType",
                                    |   "recipients": [["$recipient", "$amount"]],
                                    |   "sender": ["$sender"],
                                    |   "changeAddress": "$sender",
                                    |   "consolidationAddress": "$sender",
                                    |   "minting": "false",
                                    |   "fee": "1",
                                    |   "data": ""
                                    | }]
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
            keyRingCurve25519.generateAttestation(sender)(Base58.decode(message).getOrThrow())
          case "PublicKeyEd25519" => keyRingEd25519.generateAttestation(sender)(Base58.decode(message).getOrThrow())
        }
        val signatures: Json = Map(
          "signatures" -> sig.asJson
        ).asJson
        rawTx.deepMerge(signatures)
      }

      (res \\ "error").isEmpty shouldBe true
      (res \\ "result").head.asObject.isDefined shouldBe true

      sigTx.value.toString
    }
  }

  def testCreateSignAssetTransfer(
    sender:         Address,
    recipient:      Address,
    assetCode:      AssetCode,
    senderPropType: String,
    amount:         Int
  ): String = {
    val requestBody = ByteString(s"""
                                    |{
                                    | "jsonrpc": "2.0",
                                    | "id": "2",
                                    | "method": "topl_rawAssetTransfer",
                                    | "params": [{
                                    |   "propositionType": "$senderPropType",
                                    |   "recipients":
                                    |   [["$recipient",
                                    |    {
                                    |      "quantity" : "$amount",
                                    |      "assetCode" : "${assetCode.toString}",
                                    |      "metadata" : "ApdGzs6uwKAhuKJQswBWoVAFjNA5B8enBKfxVbzlcQ8EnpxicpRcE9B9Bgn2LGv02kYUSA1h1181ZYeECvr",
                                    |      "type" : "Asset",
                                    |      "securityRoot" : "11111111111111111111111111111111"
                                    |    }
                                    |  ]],
                                    |   "sender": ["$sender"],
                                    |   "changeAddress": "$sender",
                                    |   "consolidationAddress": "$sender",
                                    |   "fee": "1",
                                    |   "minting": true,
                                    |   "data": ""
                                    | }]
                                    |}
      """.stripMargin)

    httpPOST(requestBody) ~> route ~> check {
      val res = parse(responseAs[String]) match { case Right(re) => re; case Left(ex) => throw ex }

      val sigTx = for {
        rawTx   <- res.hcursor.downField("result").get[Json]("rawTx")
        message <- res.hcursor.downField("result").get[String]("messageToSign")
      } yield {
        val sig = senderPropType match {
          case "PublicKeyCurve25519" =>
            keyRingCurve25519.generateAttestation(sender)(Base58.decode(message).getOrThrow())
          case "PublicKeyEd25519" => keyRingEd25519.generateAttestation(sender)(Base58.decode(message).getOrThrow())
        }
        val signatures: Json = Map(
          "signatures" -> sig.asJson
        ).asJson
        rawTx.deepMerge(signatures)
      }

      (res \\ "error").isEmpty shouldBe true
      (res \\ "result").head.asObject.isDefined shouldBe true

      sigTx.getOrElse("").toString
    }
  }
}
