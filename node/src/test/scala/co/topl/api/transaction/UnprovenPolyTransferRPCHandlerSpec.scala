package co.topl.api.transaction

import akka.util.ByteString
import cats.implicits._
import co.topl.api.RPCMockState
import co.topl.attestation.{Address, PublicKeyPropositionCurve25519}
import co.topl.modifier.box.SimpleValue
import io.circe.parser.parse
import io.circe.{ACursor, Decoder, HCursor}
import org.scalatest.matchers.should.Matchers

class UnprovenPolyTransferRPCHandlerSpec extends RPCMockState with Matchers {

  import UnprovenPolyTransferRPCHandlerSpec._

  val propositionType: String = PublicKeyPropositionCurve25519.typeString
  val amount = 100
  val fee = 1

  var sender: Address = _
  var recipient: Address = _

  override def beforeAll(): Unit = {
    super.beforeAll()

    sender = keyRingCurve25519.addresses.head
    recipient = keyRingCurve25519.addresses.head
  }

  "Unproven Poly Transfer RPC Handler" should {

    "successfully create a transfer with a 'PolyTransfer' tx-type" in {
      val requestBody = createRequestBody(propositionType, sender, recipient, amount, fee)

      val path = (cursor: HCursor) => cursor.downField("result").downField("rawTx").downField("txType")

      val result =
        httpPOST(requestBody) ~> route ~> check(traverseJsonPath[String](responseAs[String], path))

      result shouldBe Right("PolyTransfer")
    }

    "successfully create a transfer with the provided sender in the 'from' field" in {
      val requestBody = createRequestBody(propositionType, sender, recipient, amount, fee)

      val path = (cursor: HCursor) => cursor.downField("result").downField("rawTx").downField("from")

      val result =
        httpPOST(requestBody) ~> route ~> check(
          traverseJsonPath[List[(String, String)]](responseAs[String], path)
        )

      result.map(_.head._1) shouldBe Right(sender.toString)
    }

    "successfully create a transfer with 'minting' set to false" in {
      val requestBody = createRequestBody(propositionType, sender, recipient, amount, fee)

      val path = (cursor: HCursor) => cursor.downField("result").downField("rawTx").downField("minting")

      val result =
        httpPOST(requestBody) ~> route ~> check(
          traverseJsonPath[Boolean](responseAs[String], path)
        )

      result shouldBe Right(false)
    }

    "successfully create a transfer with expected proposition-type" in {
      val requestBody = createRequestBody(propositionType, sender, recipient, amount, fee)

      val path = (cursor: HCursor) => cursor.downField("result").downField("rawTx").downField("propositionType")

      val result =
        httpPOST(requestBody) ~> route ~> check(
          traverseJsonPath[String](responseAs[String], path)
        )

      result shouldBe Right(propositionType)
    }

    "successfully create a transfer with recipient in 'to' field" in {
      val requestBody = createRequestBody(propositionType, sender, recipient, amount, fee)

      val path = (cursor: HCursor) => cursor.downField("result").downField("rawTx").downField("to")

      val result =
        httpPOST(requestBody) ~> route ~> check(
          traverseJsonPath[List[(String, SimpleValue)]](responseAs[String], path)
        )

      val searchResult =
        result
          .flatMap(recipients =>
            recipients
              .find(_._1 == sender.toString)
              .map(_._1)
              .toRight("sender address not found")
          )

      searchResult shouldBe Right(sender.toString)
    }

    "fail to create a transfer when sender has no polys" in {
      val emptySender = addressGen.sample.get

      val requestBody = createRequestBody(propositionType, emptySender, recipient, amount, fee)

      val path = (cursor: HCursor) => cursor.downField("error").downField("data").downField("message")

      val result = httpPOST(requestBody) ~> route ~> check(traverseJsonPath[String](responseAs[String], path))

      result shouldBe Right("EmptyPolyInputs")
    }

    "fail to create a transfer when no sender is provided" in {
      val requestBody =
        ByteString(s"""
          |{
          | "jsonrpc": "2.0",
          | "id": "2",
          | "method": "topl_rawPolyTransfer",
          | "params": [{
          |   "propositionType": "$propositionType",
          |   "recipients": [["$recipient", "$amount"]],
          |   "sender": [],
          |   "changeAddress": "$sender",
          |   "consolidationAddress": "$sender",
          |   "minting": "false",
          |   "fee": "$fee",
          |   "data": "",
          |   "boxSelectionAlgorithm": "All"
          | }]
          |}
      """.stripMargin)

      val path = (cursor: HCursor) => cursor.downField("error").downField("message")

      val result = httpPOST(requestBody) ~> route ~> check {
        val json = responseAs[String]
        traverseJsonPath[String](json, path)
      }

      result shouldBe Right("Invalid method parameter(s)")
    }

    "fail to create a transfer when send amount is negative" in {
      val negativeAmount = -100

      val requestBody = createRequestBody(propositionType, sender, recipient, negativeAmount, fee)

      val path = (cursor: HCursor) => cursor.downField("error").downField("message")

      val result = httpPOST(requestBody) ~> route ~> check {
        val json = responseAs[String]
        traverseJsonPath[String](json, path)
      }

      result shouldBe Right("Could not validate transaction")
    }
  }
}

object UnprovenPolyTransferRPCHandlerSpec {

  /**
   * Creates an Unproven Poly Transfer request HTTP body.
   * @param propositionType the type of proposition used for signing the transfer
   * @param sender the address that polys should be sent from
   * @param recipient the recipient of the polys
   * @param amount the amount of polys to send
   * @param fee the fee provided for the transaction
   * @return a [[ByteString]] representing the HTTP body
   */
  def createRequestBody(
    propositionType: String,
    sender:          Address,
    recipient:       Address,
    amount:          Int,
    fee:             Int
  ): ByteString =
    ByteString(s"""
      |{
      | "jsonrpc": "2.0",
      | "id": "2",
      | "method": "topl_rawPolyTransfer",
      | "params": [{
      |   "propositionType": "$propositionType",
      |   "recipients": [["$recipient", "$amount"]],
      |   "sender": ["$sender"],
      |   "changeAddress": "$sender",
      |   "consolidationAddress": "$sender",
      |   "minting": "false",
      |   "fee": "$fee",
      |   "data": "",
      |   "boxSelectionAlgorithm": "All"
      | }]
      |}
    """.stripMargin)

  /**
   * Traverses the provided json [[String]] using the provided cursor path.
   * @param json the JSON to parse and traverse
   * @param path the path to follow down the JSON tree
   * @tparam T the type of value to attempt to decode at the leaf of the path
   * @return if successful, a value of [[T]], otherwise a [[String]]
   */
  def traverseJsonPath[T: Decoder](json: String, path: HCursor => ACursor): Either[String, T] =
    for {
      json   <- parse(json).leftMap(_.toString)
      result <- path(json.hcursor).as[T].leftMap(_.message)
    } yield result
}
