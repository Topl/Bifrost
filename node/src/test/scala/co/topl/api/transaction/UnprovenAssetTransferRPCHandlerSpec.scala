package co.topl.api.transaction

import akka.util.ByteString
import cats.data.NonEmptyChain
import co.topl.api.RPCMockState
import co.topl.attestation.implicits._
import co.topl.codecs.json.tetra.instances._
import co.topl.models.utility.HasLength.instances.{bigIntLength, latin1DataLength}
import co.topl.models.utility.Sized
import co.topl.models.utility.StringDataTypes.Latin1Data
import co.topl.models.{Box, BoxReference, Bytes, DionAddress, NetworkPrefix, Transaction}
import io.circe.HCursor
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import io.circe.syntax._

class UnprovenAssetTransferRPCHandlerSpec extends RPCMockState with Matchers with EitherValues {

  import UnprovenAssetTransferRPCHandlerSpec._

  val fee = 1

  var sender: DionAddress = _
  var recipient: DionAddress = _
  var assetCode: Box.Values.Asset.Code = _

  var assetValue: Box.Values.Asset = _

  implicit val tetraNetworkPrefix: NetworkPrefix = NetworkPrefix(networkPrefix)

  override def beforeAll(): Unit = {
    super.beforeAll()

    sender = keyRingCurve25519.addresses.head.toDionAddress.toOption.get
    recipient = keyRingCurve25519.addresses.head.toDionAddress.toOption.get

    assetCode = Box.Values.Asset.Code(
      1.toByte,
      keyRingCurve25519.addresses.head.toDionAddress.toOption.get,
      Sized.maxUnsafe(Latin1Data.unsafe("test"))
    )

    assetValue = Box.Values.Asset(
      Sized.maxUnsafe(BigInt(1000)),
      assetCode,
      securityRoot = Bytes.fill(32)(1),
      metadata = None
    )
  }

  "Unproven Asset Transfer RPC Handler" should {

    "successfully create a transfer with the provided sender in the 'inputs' field" in {
      val requestBody =
        createRequestBody(List(sender), List(recipient -> assetValue), fee, sender, sender, None, minting = true)

      val path = (cursor: HCursor) => cursor.downField("result").downField("unprovenTransfer").downField("inputs")

      val result =
        httpPOST(requestBody) ~> route ~> check(
          traverseJsonPath[NonEmptyChain[BoxReference]](responseAs[String], path)
        )

      result.map(_.head._1).value shouldBe sender
    }

    "successfully create a transfer with 'minting' set to true" in {
      val requestBody =
        createRequestBody(List(sender), List(recipient -> assetValue), fee, sender, sender, None, minting = true)

      val path = (cursor: HCursor) => cursor.downField("result").downField("unprovenTransfer").downField("minting")

      val result =
        httpPOST(requestBody) ~> route ~> check(
          traverseJsonPath[Boolean](responseAs[String], path)
        )

      result.value shouldBe true
    }

    "successfully create a transfer with recipient in 'coinOutputs' field" in {
      val requestBody =
        createRequestBody(List(sender), List(recipient -> assetValue), fee, sender, sender, None, minting = true)

      val path = (cursor: HCursor) => cursor.downField("result").downField("unprovenTransfer").downField("coinOutputs")

      val result =
        httpPOST(requestBody) ~> route ~> check(
          traverseJsonPath[List[Transaction.CoinOutput]](responseAs[String], path)
        )

      val outputAddresses =
        result.map(outputs =>
          outputs.flatMap {
            case Transaction.AssetOutput(dionAddress, _) => List(dionAddress)
            case _                                       => List.empty
          }
        )

      outputAddresses.value should contain(recipient)
    }

    "successfully create a transfer with the expected fee change address" in {
      val requestBody =
        createRequestBody(List(sender), List(recipient -> assetValue), fee, sender, sender, None, minting = true)

      val path = (cursor: HCursor) =>
        cursor.downField("result").downField("unprovenTransfer").downField("feeOutput").downField("dionAddress")

      val result =
        httpPOST(requestBody) ~> route ~> check(
          traverseJsonPath[String](responseAs[String], path)
        )

      result.value.asJson shouldBe sender.asJson
    }

    "successfully create a transfer with the expected 'data'" in {
      val data = "test-data"

      val requestBody =
        createRequestBody(List(sender), List(recipient -> assetValue), fee, sender, sender, Some(data), minting = true)

      val path = (cursor: HCursor) => cursor.downField("result").downField("unprovenTransfer").downField("data")

      val result =
        httpPOST(requestBody) ~> route ~> check(
          traverseJsonPath[Option[String]](responseAs[String], path)
        )

      result.value.get shouldBe data
    }

    "fail to create a transfer when sender has no assets" in {
      val requestBody =
        createRequestBody(List(sender), List(recipient -> assetValue), fee, sender, sender, None, minting = false)

      val path = (cursor: HCursor) => cursor.downField("error").downField("data").downField("message")

      val result = httpPOST(requestBody) ~> route ~> check(traverseJsonPath[String](responseAs[String], path))

      result.value.contains(s"insufficient assets") shouldBe true
    }

    "fail to create a transfer when no sender is provided" in {
      val requestBody =
        createRequestBody(List.empty, List(recipient -> assetValue), fee, sender, sender, None, minting = true)

      val path = (cursor: HCursor) => cursor.downField("error").downField("message")

      val result = httpPOST(requestBody) ~> route ~> check {
        val json = responseAs[String]
        traverseJsonPath[String](json, path)
      }

      result.value shouldBe "Invalid method parameter(s)"
    }

    "fail to create a transfer when send amount is negative" in {
      val negativeAssetValue = assetValue.copy(quantity = Sized.maxUnsafe(BigInt(-1000)))

      val requestBody =
        createRequestBody(
          List(sender),
          List(recipient -> negativeAssetValue),
          fee,
          sender,
          sender,
          None,
          minting = true
        )

      val path = (cursor: HCursor) => cursor.downField("error").downField("message")

      val result = httpPOST(requestBody) ~> route ~> check {
        val json = responseAs[String]
        traverseJsonPath[String](json, path)
      }

      result.value shouldBe "Could not validate transaction"
    }
  }
}

object UnprovenAssetTransferRPCHandlerSpec {

  def createRequestBody(
    senders:            List[DionAddress],
    recipients:         List[(DionAddress, Box.Values.Asset)],
    fee:                Int,
    feeChangeAddress:   DionAddress,
    assetChangeAddress: DionAddress,
    data:               Option[String],
    minting:            Boolean
  ): ByteString = {
    val sendersString = senders.map(_.asJson).mkString(", ")

    val recipientsString =
      recipients
        .map(value => s"""
             |{
             |  "dionAddress": ${value._1.asJson},
             |  "value": ${value._2.asJson.noSpaces}
             |}""".stripMargin)
        .mkString(", ")

    val dataString = data.fold("null")(value => s""""$value"""")

    ByteString(s"""
      |{
      | "jsonrpc": "2.0",
      | "id": "2",
      | "method": "topl_unprovenAssetTransfer",
      | "params": [ {
      |   "senders": [$sendersString],
      |   "recipients": [$recipientsString],
      |   "fee": $fee,
      |   "feeChangeAddress": ${feeChangeAddress.asJson},
      |   "assetChangeAddress": ${assetChangeAddress.asJson},
      |   "data": $dataString,
      |   "minting": $minting,
      |   "boxSelectionAlgorithm": "All"
      | } ]
      |}
    """.stripMargin)
  }
}
