package co.topl.api.transaction

import akka.util.ByteString
import cats.data.NonEmptyChain
import co.topl.api.RPCMockState
import co.topl.attestation.implicits._
import co.topl.codecs.json.tetra.instances._
import co.topl.models.{BoxReference, DionAddress, NetworkPrefix, Transaction}
import io.circe.HCursor
import io.circe.syntax._
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers

class UnprovenPolyTransferRPCHandlerSpec extends RPCMockState with Matchers with EitherValues {

  import UnprovenPolyTransferRPCHandlerSpec._

  val amount = 100
  val fee = 1

  var sender: DionAddress = _
  var recipient: DionAddress = _

  implicit val tetraNetworkPrefix: NetworkPrefix = NetworkPrefix(networkPrefix)

  override def beforeAll(): Unit = {
    super.beforeAll()

    sender = keyRingCurve25519.addresses.head.toDionAddress.toOption.get
    recipient = keyRingCurve25519.addresses.head.toDionAddress.toOption.get
  }

  "Unproven Poly Transfer RPC Handler" should {

    "successfully create a transfer with the provided sender in the 'inputs' field" in {
      val requestBody = createRequestBody(List(sender), List(recipient -> amount), fee, sender, None)

      val path = (cursor: HCursor) => cursor.downField("result").downField("unprovenTransfer").downField("inputs")

      val result =
        httpPOST(requestBody) ~> route ~> check(
          traverseJsonPath[NonEmptyChain[BoxReference]](responseAs[String], path)
        )

      result.map(_.head._1).value shouldBe sender
    }

    "successfully create a transfer with 'minting' set to false" in {
      val requestBody = createRequestBody(List(sender), List(recipient -> amount), fee, sender, None)

      val path = (cursor: HCursor) => cursor.downField("result").downField("unprovenTransfer").downField("minting")

      val result =
        httpPOST(requestBody) ~> route ~> check(
          traverseJsonPath[Boolean](responseAs[String], path)
        )

      result.value shouldBe false
    }

    "successfully create a transfer with recipient in 'coinOutputs' field" in {
      val requestBody = createRequestBody(List(sender), List(recipient -> amount), fee, sender, None)

      val path = (cursor: HCursor) => cursor.downField("result").downField("unprovenTransfer").downField("coinOutputs")

      val result =
        httpPOST(requestBody) ~> route ~> check(
          traverseJsonPath[List[Transaction.CoinOutput]](responseAs[String], path)
        )

      val outputAddresses =
        result.map(outputs =>
          outputs.flatMap {
            case Transaction.PolyOutput(dionAddress, _) => List(dionAddress.asJson)
            case _                                      => List.empty
          }
        )

      outputAddresses.value should contain(recipient.asJson)
    }

    "successfully create a transfer with the expected change address" in {
      val requestBody = createRequestBody(List(sender), List(recipient -> amount), fee, sender, None)

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

      val requestBody = createRequestBody(List(sender), List(recipient -> amount), fee, sender, Some(data))

      val path = (cursor: HCursor) => cursor.downField("result").downField("unprovenTransfer").downField("data")

      val result =
        httpPOST(requestBody) ~> route ~> check(
          traverseJsonPath[Option[String]](responseAs[String], path)
        )

      result.value.get shouldBe data
    }

    "fail to create a transfer when sender has no polys" in {
      val emptySender = addressGen.sample.get.toDionAddress.toOption.get

      val requestBody = createRequestBody(List(emptySender), List(recipient -> amount), fee, sender, None)

      val path = (cursor: HCursor) => cursor.downField("error").downField("data").downField("message")

      val result = httpPOST(requestBody) ~> route ~> check(traverseJsonPath[String](responseAs[String], path))

      result.value shouldBe "Failed to build transfer: no poly inputs provided"
    }

    "fail to create a transfer when no sender is provided" in {
      val requestBody = createRequestBody(List.empty, List(recipient -> amount), fee, sender, None)

      val path = (cursor: HCursor) => cursor.downField("error").downField("message")

      val result = httpPOST(requestBody) ~> route ~> check {
        val json = responseAs[String]
        traverseJsonPath[String](json, path)
      }

      result.value shouldBe "Invalid method parameter(s)"
    }

    "fail to create a transfer when send amount is negative" in {
      val negativeAmount = -100

      val requestBody = createRequestBody(List(sender), List(recipient -> negativeAmount), fee, sender, None)

      val path = (cursor: HCursor) => cursor.downField("error").downField("message")

      val result = httpPOST(requestBody) ~> route ~> check {
        val json = responseAs[String]
        traverseJsonPath[String](json, path)
      }

      result.value shouldBe "Could not validate transaction"
    }
  }
}

object UnprovenPolyTransferRPCHandlerSpec {

  /**
   * Creates an Unproven Poly Transfer request body.
   * @param senders the list of addresses sending polys
   * @param recipients pairs of addresses and how many polys they should receive
   * @param fee the fee to pay for the transaction
   * @param changeAddress the address to send poly fee change to
   * @param data transaction data
   * @return a [[ByteString]] representing the transfer request
   */
  def createRequestBody(
    senders:       List[DionAddress],
    recipients:    List[(DionAddress, Int)],
    fee:           Int,
    changeAddress: DionAddress,
    data:          Option[String]
  ): ByteString = {
    val sendersString = senders.map(_.asJson).mkString(", ")

    val recipientsString =
      recipients
        .map(value => s"""{ "dionAddress": ${value._1.asJson}, "value": "${value._2}" }""")
        .mkString(", ")

    val dataString = data.fold("null")(value => s""""$value"""")

    ByteString(s"""
      |{
      | "jsonrpc": "2.0",
      | "id": "2",
      | "method": "topl_unprovenPolyTransfer",
      | "params": [ {
      |   "senders": [$sendersString],
      |   "recipients": [$recipientsString],
      |   "fee": $fee,
      |   "changeAddress": ${changeAddress.asJson},
      |   "data": $dataString,
      |   "boxSelectionAlgorithm": "All"
      | } ]
      |}
    """.stripMargin)
  }

}
