package co.topl.api.transaction

import akka.util.ByteString
import co.topl.api.RPCMockState
import co.topl.attestation.keyManagement.PrivateKeyCurve25519
import co.topl.attestation.{
  Address,
  KnowledgeProposition,
  Proof,
  Proposition,
  PublicKeyPropositionCurve25519,
  PublicKeyPropositionEd25519,
  SignatureCurve25519,
  ThresholdPropositionCurve25519,
  ThresholdSignatureCurve25519
}
import co.topl.codecs.json._
import co.topl.modifier.box.AssetCode
import co.topl.utils.StringDataTypes.Base58Data
import io.circe.{Encoder, Json}
import io.circe.parser.parse
import io.circe.syntax._
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.immutable.ListMap

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

      (res \\ "error") should be(empty)
      (res \\ "result").head.asObject should be(defined)
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
      res should include("Invalid method parameter")
    }
  }

  def testEncodeTransfer(unprovenTransaction: Json, messageToSign: String): Unit = {
    val requestBody = ByteString(s"""
      |{
      | "jsonrpc": "2.0",
      | "id": "2",
      | "method": "topl_encodeTransfer",
      | "params": [{
      |   "unprovenTransaction": $unprovenTransaction
      | }]
      |}
      """.stripMargin)

    httpPOST(requestBody) ~> route ~> check {
      val res = parse(responseAs[String]).value
      val encodedMessage = res.hcursor.downField("result").get[String]("messageToSign").value

      encodedMessage shouldEqual messageToSign
      res.hcursor.downField("error").values shouldBe None
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
        val sigJson = senderPropType match {
          case "PublicKeyCurve25519" =>
            keyRingCurve25519
              .generateAttestation(sender)(message.value)
              .asInstanceOf[ListMap[Proposition, Proof[PublicKeyPropositionCurve25519]]]
              .asJson
          case "PublicKeyEd25519" =>
            keyRingEd25519
              .generateAttestation(sender)(message.value)
              .asInstanceOf[ListMap[Proposition, Proof[PublicKeyPropositionEd25519]]]
              .asJson
          case "ThresholdCurve25519" =>
            val thresholdProp = propsThresholdCurve25519.filter(_.address == sender).head
            val addresses = thresholdProp.pubKeyProps.toSet[PublicKeyPropositionCurve25519].map(_.address)
            val signatures = keyRingCurve25519.generateAttestation(addresses)(message.value).values.toSet
            val thresholdSignature = ThresholdSignatureCurve25519(signatures)
            ListMap[Proposition, Proof[ThresholdPropositionCurve25519]](
              thresholdProp -> thresholdSignature
            ).asJson
        }
        val signatures: Json = Map(
          "signatures" -> sigJson
        ).asJson

        testEncodeTransfer(rawTx, res.hcursor.downField("result").get[String]("messageToSign").value)

        rawTx.deepMerge(signatures)
      }

      (res \\ "error") should be(empty)
      (sigTx.value \\ "signatures").head.asObject should be(defined)

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
        val signaturesJson: Json = senderPropType match {
          case "PublicKeyCurve25519" =>
            keyRingCurve25519
              .generateAttestation(sender)(message.value)
              .asInstanceOf[ListMap[Proposition, Proof[PublicKeyPropositionCurve25519]]]
              .asJson
          case "PublicKeyEd25519" =>
            keyRingEd25519
              .generateAttestation(sender)(message.value)
              .asInstanceOf[ListMap[Proposition, Proof[PublicKeyPropositionEd25519]]]
              .asJson
          case "ThresholdCurve25519" =>
            val thresholdProp = propsThresholdCurve25519.filter(_.address == sender).head
            val addresses = thresholdProp.pubKeyProps.toSet[PublicKeyPropositionCurve25519].map(_.address)
            val signatures = keyRingCurve25519.generateAttestation(addresses)(message.value).values.toSet
            val thresholdSignature = ThresholdSignatureCurve25519(signatures)
            ListMap[Proposition, Proof[ThresholdPropositionCurve25519]](
              thresholdProp -> thresholdSignature
            ).asJson
        }
        val signatures: Json = Map(
          "signatures" -> signaturesJson
        ).asJson

        testEncodeTransfer(rawTx, res.hcursor.downField("result").get[String]("messageToSign").value)

        rawTx.deepMerge(signatures)
      }

      (res \\ "error") should be(empty)
      (sigTx.value \\ "signatures").head.asObject should be(defined)

      sigTx.value
    }
  }

  def assetTransferRequestBody(
    sender:         Address,
    recipient:      Address,
    assetCode:      String,
    senderPropType: String,
    amount:         Int,
    securityRoot:   String = "11111111111111111111111111111111"
  ): ByteString =
    ByteString(s"""
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
      |      "assetCode" : "$assetCode",
      |      "metadata" : "ApdGzs6uwKAhuKJQswBWoVAFjNA5B8enBKfxVbzlcQ8EnpxicpRcE9B9Bgn2LGv02kYUSA1h1181ZYeECvr",
      |      "type" : "Asset",
      |      "securityRoot" : "$securityRoot"
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

  def testCreateSignAssetTransfer(
    sender:         Address,
    recipient:      Address,
    assetCode:      AssetCode,
    senderPropType: String,
    amount:         Int
  ): Json = {
    val requestBody = assetTransferRequestBody(sender, recipient, assetCode.toString, senderPropType, amount)

    httpPOST(requestBody) ~> route ~> check {
      val res = parse(responseAs[String]).value

      val sigTx = for {
        rawTx   <- res.hcursor.downField("result").get[Json]("rawTx")
        message <- res.hcursor.downField("result").get[Base58Data]("messageToSign")
      } yield {
        val signaturesJson: Json = senderPropType match {
          case "PublicKeyCurve25519" =>
            keyRingCurve25519
              .generateAttestation(sender)(message.value)
              .asInstanceOf[ListMap[Proposition, Proof[PublicKeyPropositionCurve25519]]]
              .asJson
          case "PublicKeyEd25519" =>
            keyRingEd25519
              .generateAttestation(sender)(message.value)
              .asInstanceOf[ListMap[Proposition, Proof[PublicKeyPropositionEd25519]]]
              .asJson
          case "ThresholdCurve25519" =>
            val thresholdProp = propsThresholdCurve25519.filter(_.address == sender).head
            val addresses = thresholdProp.pubKeyProps.toSet[PublicKeyPropositionCurve25519].map(_.address)
            val signatures = keyRingCurve25519.generateAttestation(addresses)(message.value).values.toSet
            val thresholdSignature = ThresholdSignatureCurve25519(signatures)
            ListMap[Proposition, Proof[ThresholdPropositionCurve25519]](
              thresholdProp -> thresholdSignature
            ).asJson
        }
        val signatures: Json = Map(
          "signatures" -> signaturesJson
        ).asJson

        testEncodeTransfer(rawTx, res.hcursor.downField("result").get[String]("messageToSign").value)

        rawTx.deepMerge(signatures)
      }

      (res \\ "error") should be(empty)
      (sigTx.value \\ "signatures").head.asObject should be(defined)

      sigTx.value
    }
  }
}
