package co.topl.modifier.transaction

import co.topl.attestation._
import co.topl.modifier.BoxReader
import co.topl.modifier.box._
import co.topl.modifier.transaction.Transaction.TxType
import co.topl.modifier.transaction.TransferTransaction.{encodeFrom, getSenderBoxesForTx, BoxParams}
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.StringDataTypes.Latin1Data
import co.topl.utils.codecs.Int128Codec
import co.topl.utils.codecs.implicits._
import co.topl.utils.{Identifiable, Identifier, Int128}
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}

import java.time.Instant
import scala.collection.immutable.ListMap

case class PolyTransfer[
  P <: Proposition: EvidenceProducer: Identifiable
](
  override val from:        IndexedSeq[(Address, Box.Nonce)],
  override val to:          IndexedSeq[(Address, SimpleValue)],
  override val attestation: ListMap[P, Proof[P]],
  override val fee:         Int128,
  override val timestamp:   Long,
  override val data:        Option[Latin1Data] = None,
  override val minting:     Boolean
) extends TransferTransaction[SimpleValue, P](from, to, attestation, fee, timestamp, data, minting) {

  override val coinOutput: Iterable[PolyBox] =
    coinOutputParams.map { case BoxParams(evi, nonce, value) =>
      PolyBox(evi, nonce, value)
    }

  override val newBoxes: Iterable[TokenBox[SimpleValue]] = {
    // this only creates an output if the value of the output boxes is non-zero
    val recipientCoinOutput: Iterable[PolyBox] = coinOutput.filter(_.value.quantity > 0)
    val hasRecipientOutput: Boolean = recipientCoinOutput.nonEmpty
    val hasFeeChangeOutput: Boolean = feeChangeOutput.value.quantity > 0

    (hasRecipientOutput, hasFeeChangeOutput) match {
      case (false, false) => Iterable()
      case (false, true)  => Iterable(feeChangeOutput) // JAA - only possible because this is Poly TX
      case (true, false)  => recipientCoinOutput
      case (true, true)   => Iterable(feeChangeOutput) ++ recipientCoinOutput
    }
  }
}

object PolyTransfer {
  val typePrefix: TxType = 2: Byte
  val typeString: String = "PolyTransfer"

  implicit def identifier[P <: Proposition]: Identifiable[PolyTransfer[P]] = Identifiable.instance { () =>
    Identifier(typeString, typePrefix)
  }

  object Validation {
    sealed trait InvalidPolyTransfer

    case object InsufficientFunds extends InvalidPolyTransfer
    type InsufficientFunds = InsufficientFunds.type

    case object NoInputBoxes extends InvalidPolyTransfer
    type NoInputBoxes = NoInputBoxes.type

    case object NonUniqueInputs extends InvalidPolyTransfer
    type NonUniqueInputs = NonUniqueInputs.type

    case object NoRecipients extends InvalidPolyTransfer
    type NoRecipients = NoRecipients.type

    case object NonUniqueRecipients extends InvalidPolyTransfer
    type NonUniqueRecipients = NonUniqueRecipients.type

    type ValidationResult[T] = Either[InvalidPolyTransfer, T]

    def validateNonEmptyInputs(
      boxes: IndexedSeq[(Address, PolyBox)]
    ): ValidationResult[IndexedSeq[(Address, PolyBox)]] =
      Either.cond(boxes.nonEmpty, boxes, NoInputBoxes)

    def validateUniqueInputs(boxes: IndexedSeq[(Address, PolyBox)]): ValidationResult[IndexedSeq[(Address, PolyBox)]] =
      Either.cond(boxes.distinctBy(_._2.nonce).length == boxes.length, boxes, NonUniqueInputs)

    def validateNonEmptyRecipients(
      recipients: IndexedSeq[(Address, SimpleValue)]
    ): ValidationResult[IndexedSeq[(Address, SimpleValue)]] =
      Either.cond(recipients.nonEmpty, recipients, NoRecipients)

    def validateUniqueRecipients(
      recipients: IndexedSeq[(Address, SimpleValue)]
    ): ValidationResult[IndexedSeq[(Address, SimpleValue)]] =
      Either.cond(recipients.distinctBy(_._1).length == recipients.length, recipients, NonUniqueRecipients)

    def validateFeeFunds(funds: Int128, feeAmount: Int128): ValidationResult[Int128] =
      Either.cond(funds >= feeAmount, funds - feeAmount, InsufficientFunds)

    def validatePaymentFunds(funds: Int128, paymentAmount: Int128): ValidationResult[Int128] =
      Either.cond(funds >= paymentAmount, funds - paymentAmount, InsufficientFunds)
  }

  import Validation._

  def validated[P <: Proposition: EvidenceProducer: Identifiable](
    fromBoxes:     IndexedSeq[(Address, PolyBox)],
    recipients:    IndexedSeq[(Address, SimpleValue)],
    changeAddress: Address,
    fee:           Int128,
    data:          Option[Latin1Data],
    minting:       Boolean
  ): ValidationResult[PolyTransfer[P]] =
    for {
      _             <- validateNonEmptyInputs(fromBoxes)
      _             <- validateUniqueInputs(fromBoxes)
      _             <- validateNonEmptyRecipients(recipients)
      _             <- validateUniqueRecipients(recipients)
      amountToSpend <- validateFeeFunds(boxFunds(fromBoxes), fee)
      paymentAmount = recipients.map(_._2.quantity).sum
      changeAmount <- validatePaymentFunds(amountToSpend, paymentAmount)
      changeRecipient = changeAddress -> SimpleValue(changeAmount)
      nonZeroRecipients = (changeRecipient +: recipients).filter(_._2.quantity > 0)
      fromBoxNonces = fromBoxes.map(box => box._1 -> box._2.nonce)
    } yield PolyTransfer[P](
      fromBoxNonces,
      nonZeroRecipients,
      ListMap(), // unsigned
      fee,
      Instant.now.toEpochMilli,
      data,
      minting
    )

  @deprecated("use co.topl.modifier.transaction.builder.buildTransfer instead")
  def createRaw[P <: Proposition: EvidenceProducer: Identifiable](
    boxReader:     BoxReader[ProgramId, Address],
    recipients:    IndexedSeq[(Address, SimpleValue)],
    sender:        IndexedSeq[Address],
    changeAddress: Address,
    fee:           Int128,
    data:          Option[Latin1Data]
  ): ValidationResult[PolyTransfer[P]] = {
    val polyBoxes =
      sender
        .map(addr => addr -> boxReader.getTokenBoxes(addr).getOrElse(IndexedSeq()))
        .flatMap((senderBoxes: (Address, Seq[TokenBox[TokenValueHolder]])) => senderBoxes._2.map(senderBoxes._1 -> _))
        .foldLeft(IndexedSeq[(Address, PolyBox)]()) {
          case (polyBoxes, (addr: Address, box: PolyBox)) => polyBoxes :+ (addr -> box)
          case (polyBoxes, _)                             => polyBoxes
        }

    validated(polyBoxes, recipients, changeAddress, fee, data, false)
  }

  implicit def jsonEncoder[P <: Proposition]: Encoder[PolyTransfer[P]] = { tx: PolyTransfer[P] =>
    Map(
      "txId"            -> tx.id.asJson,
      "txType"          -> "PolyTransfer".asJson,
      "propositionType" -> tx.getPropIdentifier.typeString.asJson,
      "newBoxes"        -> tx.newBoxes.toSeq.asJson,
      "boxesToRemove"   -> tx.boxIdsToOpen.asJson,
      "from"            -> encodeFrom(tx.from),
      "to"              -> tx.to.asJson,
      "signatures"      -> tx.attestation.asJson,
      "fee"             -> tx.fee.asJson(Int128Codec.jsonEncoder),
      "timestamp"       -> tx.timestamp.asJson,
      "minting"         -> tx.minting.asJson,
      "data"            -> tx.data.asJson
    ).asJson
  }

  implicit def jsonDecoder(implicit networkPrefix: NetworkPrefix): Decoder[PolyTransfer[_ <: Proposition]] =
    (c: HCursor) =>
      for {
        from      <- c.downField("from").as[IndexedSeq[(Address, Box.Nonce)]]
        to        <- c.downField("to").as[IndexedSeq[(Address, SimpleValue)]]
        fee       <- c.get[Int128]("fee")(Int128Codec.jsonDecoder)
        timestamp <- c.downField("timestamp").as[Long]
        data      <- c.downField("data").as[Option[Latin1Data]]
        propType  <- c.downField("propositionType").as[String]
        minting   <- c.downField("minting").as[Boolean]
      } yield (propType match {
        case PublicKeyPropositionCurve25519.`typeString` =>
          c.downField("signatures").as[ListMap[PublicKeyPropositionCurve25519, SignatureCurve25519]].map {
            new PolyTransfer[PublicKeyPropositionCurve25519](from, to, _, fee, timestamp, data, minting)
          }

        case ThresholdPropositionCurve25519.`typeString` =>
          c.downField("signatures").as[ListMap[ThresholdPropositionCurve25519, ThresholdSignatureCurve25519]].map {
            new PolyTransfer[ThresholdPropositionCurve25519](from, to, _, fee, timestamp, data, minting)
          }

        case PublicKeyPropositionEd25519.`typeString` =>
          c.downField("signatures").as[ListMap[PublicKeyPropositionEd25519, SignatureEd25519]].map {
            new PolyTransfer[PublicKeyPropositionEd25519](from, to, _, fee, timestamp, data, minting)
          }
      }) match {
        case Right(tx) => tx
        case Left(ex)  => throw ex
      }
}
