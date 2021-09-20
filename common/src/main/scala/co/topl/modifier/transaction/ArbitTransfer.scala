package co.topl.modifier.transaction

import co.topl.attestation._
import co.topl.modifier.BoxReader
import co.topl.modifier.box._
import co.topl.modifier.transaction.Transaction.TxType
import co.topl.modifier.transaction.TransferTransaction.{encodeFrom, BoxParams, TransferCreationState}
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.StringDataTypes.Latin1Data
import co.topl.utils.codecs.Int128Codec
import co.topl.utils.codecs.implicits._
import co.topl.utils.{Identifiable, Identifier, Int128}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}

import java.time.Instant
import scala.collection.immutable.ListMap
import scala.util.Try

case class ArbitTransfer[
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

  override val coinOutput: Iterable[ArbitBox] =
    coinOutputParams.map { case BoxParams(evi, nonce, value) =>
      ArbitBox(evi, nonce, value)
    }

  override val newBoxes: Iterable[TokenBox[SimpleValue]] = {
    // this only creates an output if the value of the output boxes is non-zero
    val recipientCoinOutput: Iterable[ArbitBox] = coinOutput.filter(_.value.quantity > 0)
    val hasRecipientOutput: Boolean = recipientCoinOutput.nonEmpty
    val hasFeeChangeOutput: Boolean = feeChangeOutput.value.quantity > 0

    (hasRecipientOutput, hasFeeChangeOutput) match {
      case (false, _)    => Iterable()
      case (true, false) => recipientCoinOutput
      case (true, true)  => Iterable(feeChangeOutput) ++ recipientCoinOutput
    }
  }
}

object ArbitTransfer {
  val typePrefix: TxType = 1: Byte
  val typeString: String = "ArbitTransfer"

  implicit def identifier[P <: Proposition]: Identifiable[ArbitTransfer[P]] = Identifiable.instance { () =>
    Identifier(typeString, typePrefix)
  }

  object Validation {
    sealed trait InvalidArbitTransfer

    case object InsufficientFunds extends InvalidArbitTransfer

    case object EmptyPolyInputs extends InvalidArbitTransfer

    case object DuplicatePolyInputs extends InvalidArbitTransfer

    case object EmptyArbitInputs extends InvalidArbitTransfer

    case object DuplicateArbitInputs extends InvalidArbitTransfer

    case object NoRecipients extends InvalidArbitTransfer

    case object NonUniqueRecipients extends InvalidArbitTransfer

    type ValidationResult[T] = Either[InvalidArbitTransfer, T]

    def validatePolyBoxes(polyBoxes: IndexedSeq[(Address, PolyBox)]): ValidationResult[IndexedSeq[(Address, PolyBox)]] =
      for {
        _ <- Either.cond(polyBoxes.nonEmpty, polyBoxes, EmptyPolyInputs)
        _ <- Either.cond(polyBoxes.map(_._2.nonce).distinct.length == polyBoxes.length, polyBoxes, DuplicatePolyInputs)
      } yield polyBoxes

    def validateArbitBoxes(
      arbitBoxes: IndexedSeq[(Address, ArbitBox)]
    ): ValidationResult[IndexedSeq[(Address, ArbitBox)]] =
      for {
        _ <- Either.cond(arbitBoxes.nonEmpty, arbitBoxes, EmptyArbitInputs)
        _ <- Either.cond(
          arbitBoxes.map(_._2.nonce).distinct.length == arbitBoxes.length,
          arbitBoxes,
          DuplicateArbitInputs
        )
      } yield arbitBoxes

    def validateRecipients(
      recipients: IndexedSeq[(Address, SimpleValue)]
    ): ValidationResult[IndexedSeq[(Address, SimpleValue)]] =
      for {
        _ <- Either.cond(recipients.nonEmpty, recipients, NoRecipients)
        _ <- Either.cond(recipients.map(_._1).distinct.length == recipients.length, recipients, NonUniqueRecipients)
      } yield recipients

    def validateFeeFunds(funds: Int128, feeAmount: Int128): ValidationResult[Int128] =
      Either.cond(funds >= feeAmount, funds - feeAmount, InsufficientFunds)

    def validatePaymentFunds(funds: Int128, paymentAmount: Int128): ValidationResult[Int128] =
      Either.cond(funds >= paymentAmount, funds - paymentAmount, InsufficientFunds)

  }

  import Validation._

  def validated[P <: Proposition: EvidenceProducer: Identifiable](
    polyBoxes:            IndexedSeq[(Address, PolyBox)],
    arbitBoxes:           IndexedSeq[(Address, ArbitBox)],
    recipients:           IndexedSeq[(Address, SimpleValue)],
    changeAddress:        Address,
    consolidationAddress: Address,
    fee:                  Int128,
    data:                 Option[Latin1Data],
    minting:              Boolean
  ): ValidationResult[ArbitTransfer[P]] =
    for {
      _           <- validatePolyBoxes(polyBoxes)
      _           <- validateArbitBoxes(arbitBoxes)
      _           <- validateRecipients(recipients)
      change      <- validateFeeFunds(polyBoxes.map(_._2.value.quantity).sum, fee)
      arbitChange <- validatePaymentFunds(arbitBoxes.map(_._2.value.quantity).sum, recipients.map(_._2.quantity).sum)
      changeOutput = changeAddress             -> SimpleValue(change)
      arbitChangeOutput = consolidationAddress -> SimpleValue(arbitChange)
      nonZeroOutputs = (changeOutput +: arbitChangeOutput +: recipients).filter(_._2.quantity > 0)
    } yield ArbitTransfer[P](
      (polyBoxes ++ arbitBoxes).map(x => x._1 -> x._2.nonce),
      nonZeroOutputs,
      ListMap(), // unsigned tx
      fee,
      Instant.now.toEpochMilli,
      data,
      minting
    )

  @deprecated("use co.topl.modifier.transaction.builder.buildTransfer instead")
  def createRaw[
    P <: Proposition: EvidenceProducer: Identifiable
  ](
    boxReader:            BoxReader[ProgramId, Address],
    toReceive:            IndexedSeq[(Address, SimpleValue)],
    sender:               IndexedSeq[Address],
    changeAddress:        Address,
    consolidationAddress: Address,
    fee:                  Int128,
    data:                 Option[Latin1Data]
  ): ValidationResult[ArbitTransfer[P]] = {
    val (polyBoxes, arbitBoxes) =
      sender
        .map(addr => addr -> boxReader.getTokenBoxes(addr).getOrElse(IndexedSeq()))
        .flatMap((senderBoxes: (Address, Seq[TokenBox[TokenValueHolder]])) => senderBoxes._2.map(senderBoxes._1 -> _))
        .foldLeft((IndexedSeq[(Address, PolyBox)](), IndexedSeq[(Address, ArbitBox)]())) {
          case ((polyBoxes, arbitBoxes), (addr: Address, box: PolyBox))  => (polyBoxes :+ (addr -> box), arbitBoxes)
          case ((polyBoxes, arbitBoxes), (addr: Address, box: ArbitBox)) => (polyBoxes, arbitBoxes :+ (addr -> box))
          case (boxes, _)                                                => boxes
        }

    validated(
      polyBoxes,
      arbitBoxes,
      toReceive,
      changeAddress,
      consolidationAddress,
      fee,
      data,
      false
    )
  }

  implicit def jsonEncoder[P <: Proposition]: Encoder[ArbitTransfer[P]] = { tx: ArbitTransfer[P] =>
    Map(
      "txId"            -> tx.id.asJson,
      "txType"          -> "ArbitTransfer".asJson,
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

  implicit def jsonDecoder(implicit networkPrefix: NetworkPrefix): Decoder[ArbitTransfer[_ <: Proposition]] =
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
            new ArbitTransfer[PublicKeyPropositionCurve25519](from, to, _, fee, timestamp, data, minting)
          }

        case ThresholdPropositionCurve25519.`typeString` =>
          c.downField("signatures").as[ListMap[ThresholdPropositionCurve25519, ThresholdSignatureCurve25519]].map {
            new ArbitTransfer[ThresholdPropositionCurve25519](from, to, _, fee, timestamp, data, minting)
          }

        case PublicKeyPropositionEd25519.`typeString` =>
          c.downField("signatures").as[ListMap[PublicKeyPropositionEd25519, SignatureEd25519]].map {
            new ArbitTransfer[PublicKeyPropositionEd25519](from, to, _, fee, timestamp, data, minting)
          }
      }) match {
        case Right(tx) => tx
        case Left(ex)  => throw ex
      }
}
