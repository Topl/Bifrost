package co.topl.api.transaction

import akka.util.ByteString
import co.topl.api.RPCMockState
import co.topl.attestation.{Address, PublicKeyPropositionCurve25519, ThresholdSignatureCurve25519}
import co.topl.modifier.box.AssetCode
import co.topl.utils.StringDataTypes.Base58Data
import co.topl.utils.codecs.implicits.base58JsonDecoder
import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax._
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

trait TransferRPCTestMethods extends AnyWordSpec with Matchers with RPCMockState with EitherValues {

  def testBroadcastTx(tx: Json): Unit = {
    val requestBody = ByteString(s"""
      |{
      | "jsonrpc": "2.0",
      | "id": "2",
      | "method": "topl_broadcastTx",
      | "params": [{
      |   "tx": ${tx.toString}
      | }]
      |}
      |""".stripMargin)

    httpPOST(requestBody) ~> route ~> check {
      val res = parse(responseAs[String]) match { case Right(re) => re; case Left(ex) => throw ex }

      (res \\ "error").isEmpty shouldBe true
      (res \\ "result").head.asObject.isDefined shouldBe true
    }
  }

  def testBroadcastTxInvalidProp(tx: Json): Unit = {
    val requestBody = ByteString(s"""
      |{
      | "jsonrpc": "2.0",
      | "id": "2",
      | "method": "topl_broadcastTx",
      | "params": [{
      |   "tx": ${tx.toString}
      | }]
      |}
      |""".stripMargin)

    httpPOST(requestBody) ~> route ~> check {
      val res = parse(responseAs[String]).value.hcursor.downField("error").as[Json].toString
      res should include("Invalid proposition generation")
    }
  }

  def testCreateSignPolyTransfer(sender: Address, recipient: Address, senderPropType: String, amount: Int): Json = {
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
      |   "data": "",
      |   "boxSelectionAlgorithm": "All"
      | }]
      |}
      """.stripMargin)

    httpPOST(requestBody) ~> route ~> check {
      val res = parse(responseAs[String]).value

      val sigTx = for {
        rawTx   <- res.hcursor.downField("result").get[Json]("rawTx")
        message <- res.hcursor.downField("result").get[Base58Data]("messageToSign")
      } yield {
        val sig = senderPropType match {
          case "PublicKeyCurve25519" => keyRingCurve25519.generateAttestation(sender)(message.value)
          case "PublicKeyEd25519"    => keyRingEd25519.generateAttestation(sender)(message.value)
          case "ThresholdCurve25519" =>
            val thresholdProp = propsThresholdCurve25519.filter(_.address == sender).head
            val addresses = thresholdProp.pubKeyProps.toSet[PublicKeyPropositionCurve25519].map(_.address)
            val signatures = keyRingCurve25519.generateAttestation(addresses)(message.value).values.toSet
            val thresholdSignature = ThresholdSignatureCurve25519(signatures)
            Map(thresholdProp -> thresholdSignature)
        }
        val signatures: Json = Map(
          "signatures" -> sig.asJson
        ).asJson
        rawTx.deepMerge(signatures)
      }

      (res \\ "error").isEmpty shouldBe true
      (sigTx.value \\ "signatures").head.asObject.isDefined shouldBe true

      sigTx.value
    }
  }

  def testCreateSignArbitTransfer(sender: Address, recipient: Address, senderPropType: String, amount: Int): Json = {
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
                                    |   "data": "",
                                    |   "boxSelectionAlgorithm": "All"
                                    | }]
                                    |}
      """.stripMargin)

    httpPOST(requestBody) ~> route ~> check {
      val res = parse(responseAs[String]).value

      val sigTx = for {
        rawTx   <- res.hcursor.downField("result").get[Json]("rawTx")
        message <- res.hcursor.downField("result").get[Base58Data]("messageToSign")
      } yield {
        val sig = senderPropType match {
          case "PublicKeyCurve25519" => keyRingCurve25519.generateAttestation(sender)(message.value)
          case "PublicKeyEd25519"    => keyRingEd25519.generateAttestation(sender)(message.value)
          case "ThresholdCurve25519" =>
            val thresholdProp = propsThresholdCurve25519.filter(_.address == sender).head
            val addresses = thresholdProp.pubKeyProps.toSet[PublicKeyPropositionCurve25519].map(_.address)
            val signatures = keyRingCurve25519.generateAttestation(addresses)(message.value).values.toSet
            val thresholdSignature = ThresholdSignatureCurve25519(signatures)
            Map(thresholdProp -> thresholdSignature)
        }
        val signatures: Json = Map(
          "signatures" -> sig.asJson
        ).asJson
        rawTx.deepMerge(signatures)
      }

      (res \\ "error").isEmpty shouldBe true
      (sigTx.value \\ "signatures").head.asObject.isDefined shouldBe true

      sigTx.value
    }
  }

  def testCreateSignAssetTransfer(
    sender:         Address,
    recipient:      Address,
    assetCode:      AssetCode,
    senderPropType: String,
    amount:         Int
  ): Json = {
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
                                    |   "data": "",
                                    |   "boxSelectionAlgorithm": "All"
                                    | }]
                                    |}
      """.stripMargin)

    httpPOST(requestBody) ~> route ~> check {
      val res = parse(responseAs[String]).value

      val sigTx = for {
        rawTx   <- res.hcursor.downField("result").get[Json]("rawTx")
        message <- res.hcursor.downField("result").get[Base58Data]("messageToSign")
      } yield {
        val sig = senderPropType match {
          case "PublicKeyCurve25519" => keyRingCurve25519.generateAttestation(sender)(message.value)
          case "PublicKeyEd25519"    => keyRingEd25519.generateAttestation(sender)(message.value)
          case "ThresholdCurve25519" =>
            val thresholdProp = propsThresholdCurve25519.filter(_.address == sender).head
            val addresses = thresholdProp.pubKeyProps.toSet[PublicKeyPropositionCurve25519].map(_.address)
            val signatures = keyRingCurve25519.generateAttestation(addresses)(message.value).values.toSet
            val thresholdSignature = ThresholdSignatureCurve25519(signatures)
            Map(thresholdProp -> thresholdSignature)
        }
        val signatures: Json = Map(
          "signatures" -> sig.asJson
        ).asJson
        rawTx.deepMerge(signatures)
      }

      (res \\ "error").isEmpty shouldBe true
      (sigTx.value \\ "signatures").head.asObject.isDefined shouldBe true

      sigTx.value
    }
  }
}
