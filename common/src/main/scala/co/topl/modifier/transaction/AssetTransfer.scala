package co.topl.modifier.transaction

import cats.implicits._
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
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}

import java.time.Instant
import scala.util.Try
import scala.Iterable
import scala.collection.immutable.ListMap

case class AssetTransfer[
  P <: Proposition: EvidenceProducer: Identifiable
](
  override val from:        IndexedSeq[(Address, Box.Nonce)],
  override val to:          IndexedSeq[(Address, TokenValueHolder)],
  override val attestation: ListMap[P, Proof[P]],
  override val fee:         Int128,
  override val timestamp:   Long,
  override val data:        Option[Latin1Data] = None,
  override val minting:     Boolean = false
) extends TransferTransaction[TokenValueHolder, P](from, to, attestation, fee, timestamp, data, minting) {

  override val coinOutput: Iterable[AssetBox] =
    coinOutputParams.map { case BoxParams(evi, nonce, value: AssetValue) =>
      AssetBox(evi, nonce, value)
    }

  override val newBoxes: Iterable[TokenBox[TokenValueHolder]] = {
    // this only creates an output if the value of the output boxes is non-zero
    val recipientCoinOutput: Iterable[AssetBox] = coinOutput.filter(_.value.quantity > 0)
    val hasRecipientOutput: Boolean = recipientCoinOutput.nonEmpty
    val hasFeeChangeOutput: Boolean = feeChangeOutput.value.quantity > 0

    (hasRecipientOutput, hasFeeChangeOutput) match {
      case (false, _)    => Iterable()
      case (true, false) => recipientCoinOutput
      case (true, true)  => Iterable(feeChangeOutput) ++ recipientCoinOutput
    }
  }
}

object AssetTransfer {
  val typePrefix: TxType = 3: Byte
  val typeString: String = "AssetTransfer"

  implicit def identifier[P <: Proposition]: Identifiable[AssetTransfer[P]] = Identifiable.instance { () =>
    Identifier(typeString, typePrefix)
  }

  object Validation {
    sealed trait InvalidAssetTransfer

    case object EmptyPolyInputs extends InvalidAssetTransfer
    type EmptyPolyInputs = EmptyPolyInputs.type

    case object DuplicatePolyInputs extends InvalidAssetTransfer
    type DuplicatePolyInputs = DuplicatePolyInputs.type

    case object DuplicateAssetInputs extends InvalidAssetTransfer
    type DuplicateAssetInputs = DuplicateAssetInputs.type

    case object DuplicateAssetCodes extends InvalidAssetTransfer
    type DuplicateAssetCodes = DuplicateAssetCodes.type

    case object DifferentInputOutputCodes extends InvalidAssetTransfer
    type DifferentInputOutputCodes = DifferentInputOutputCodes.type

    case object EmptyAssetInputs extends InvalidAssetTransfer
    type EmptyAssetInputs = EmptyAssetInputs.type

    case object EmptyRecipients extends InvalidAssetTransfer
    type EmptyRecipients = EmptyRecipients.type

    case object DuplicateRecipients extends InvalidAssetTransfer
    type DuplicateRecipients = DuplicateRecipients.type

    case object InsufficientPolyFunds extends InvalidAssetTransfer
    type InsufficientPolyFunds = InsufficientPolyFunds.type

    case object InsufficientAssetFunds extends InvalidAssetTransfer
    type InsufficientAssetFunds = InsufficientAssetFunds.type

    type ValidationResult[T] = Either[InvalidAssetTransfer, T]

    def validatePolyInputs(
      polyBoxes: IndexedSeq[(Address, PolyBox)]
    ): ValidationResult[IndexedSeq[(Address, PolyBox)]] =
      for {
        _ <- Either.cond(polyBoxes.nonEmpty, polyBoxes, EmptyPolyInputs)
        _ <- Either.cond(polyBoxes.distinctBy(_._2.nonce).length == polyBoxes.length, polyBoxes, DuplicatePolyInputs)
      } yield polyBoxes

    def validateAssetInputs(
      assetBoxes: IndexedSeq[(Address, AssetBox)],
      minting:    Boolean
    ): ValidationResult[IndexedSeq[(Address, AssetBox)]] =
      if (!minting)
        for {
          _ <- Either.cond(assetBoxes.nonEmpty, assetBoxes, EmptyAssetInputs)
          _ <- Either.cond(assetBoxes.distinctBy(_._2.nonce).length == 1, assetBoxes, DuplicateAssetInputs)
          _ <- Either.cond(assetBoxes.distinctBy(_._2.value.assetCode).length == 1, assetBoxes, DuplicateAssetCodes)
        } yield assetBoxes
      else assetBoxes.asRight

    def validateRecipients(
      recipients: IndexedSeq[(Address, AssetValue)]
    ): ValidationResult[IndexedSeq[(Address, AssetValue)]] =
      for {
        _ <- Either.cond(recipients.nonEmpty, recipients, EmptyRecipients)
        _ <- Either.cond(recipients.distinctBy(_._1).length == recipients.length, recipients, DuplicateRecipients)
        _ <- Either.cond(recipients.distinctBy(_._2.assetCode).length == 1, recipients, DuplicateAssetCodes)
      } yield recipients

    def validateSameAssetCode(
      expectedAssetCode: AssetCode,
      assetBoxes:        IndexedSeq[(Address, AssetBox)],
      minting:           Boolean
    ): ValidationResult[AssetCode] =
      if (!minting)
        Either.cond(
          assetBoxes.head._2.value.assetCode == expectedAssetCode,
          expectedAssetCode,
          DifferentInputOutputCodes
        )
      else
        expectedAssetCode.asRight

    def validateFeeFunds(funds: Int128, feeAmount: Int128): ValidationResult[Int128] =
      Either.cond(funds >= feeAmount, funds - feeAmount, InsufficientPolyFunds)

    def validateAssetPaymentFunds(funds: Int128, paymentAmount: Int128, minting: Boolean): ValidationResult[Int128] =
      if (!minting) Either.cond(funds >= paymentAmount, funds - paymentAmount, InsufficientAssetFunds)
      else Int128(0).asRight
  }

  import Validation._

  def validated[P <: Proposition: EvidenceProducer: Identifiable](
    polyBoxes:            IndexedSeq[(Address, PolyBox)],
    assetBoxes:           IndexedSeq[(Address, AssetBox)],
    recipients:           IndexedSeq[(Address, AssetValue)],
    changeAddress:        Address,
    consolidationAddress: Address,
    fee:                  Int128,
    data:                 Option[Latin1Data],
    minting:              Boolean
  ): ValidationResult[AssetTransfer[P]] =
    for {
      _ <- validatePolyInputs(polyBoxes)
      _ <- validateAssetInputs(assetBoxes, minting)
      _ <- validateRecipients(recipients)
      assetCode = recipients.head._2.assetCode
      _          <- validateSameAssetCode(assetCode, assetBoxes, minting)
      polyChange <- validateFeeFunds(boxFunds(polyBoxes), fee)
      assetPayment = recipients.map(_._2.quantity).sum
      assetChange <- validateAssetPaymentFunds(boxFunds(assetBoxes), assetPayment, minting)
      assetBoxNonces = assetBoxes.map(box => box._1 -> box._2.nonce)
      polyBoxNonces = polyBoxes.map(box => box._1 -> box._2.nonce)
      changeOutput = changeAddress       -> SimpleValue(polyChange)
      assetOutput = consolidationAddress -> AssetValue(assetChange, assetCode)
      nonZeroOutputs = (IndexedSeq(changeOutput, assetOutput) ++ recipients).filter(_._2.quantity > 0)
    } yield AssetTransfer[P](
      polyBoxNonces ++ assetBoxNonces,
      nonZeroOutputs,
      ListMap(), // unsigned tx
      fee,
      Instant.now.toEpochMilli,
      data,
      minting
    )

  def createRaw[P <: Proposition: EvidenceProducer: Identifiable](
    boxReader:            BoxReader[ProgramId, Address],
    recipients:           IndexedSeq[(Address, AssetValue)],
    sender:               IndexedSeq[Address],
    changeAddress:        Address,
    consolidationAddress: Address,
    fee:                  Int128,
    data:                 Option[Latin1Data],
    minting:              Boolean
  ): ValidationResult[AssetTransfer[P]] =
    for {
      assetCode <- recipients.headOption.map(_._2.assetCode.asRight).getOrElse(EmptyRecipients.asLeft)
      (polyBoxes, assetBoxes) =
        sender
          .map(addr => addr -> boxReader.getTokenBoxes(addr).getOrElse(IndexedSeq()))
          .flatMap((senderBoxes: (Address, Seq[TokenBox[TokenValueHolder]])) => senderBoxes._2.map(senderBoxes._1 -> _))
          .foldLeft((IndexedSeq[(Address, PolyBox)](), IndexedSeq[(Address, AssetBox)]())) {
            case ((polyBoxes, assetBoxes), (addr: Address, box: PolyBox))  => (polyBoxes :+ (addr -> box), assetBoxes)
            case ((polyBoxes, assetBoxes), (addr: Address, box: AssetBox)) => (polyBoxes, assetBoxes :+ (addr -> box))
            case (boxes, _) => boxes
          }
      assetTransfer <- validated(
        polyBoxes,
        assetBoxes,
        recipients,
        changeAddress,
        consolidationAddress,
        fee,
        data,
        minting
      )
    } yield assetTransfer

  implicit def jsonEncoder[P <: Proposition]: Encoder[AssetTransfer[P]] = { tx: AssetTransfer[P] =>
    Map(
      "txId"            -> tx.id.asJson,
      "txType"          -> "AssetTransfer".asJson,
      "propositionType" -> tx.getPropIdentifier.typeString.asJson,
      "newBoxes"        -> tx.newBoxes.toSeq.asJson,
      "boxesToRemove"   -> tx.boxIdsToOpen.asJson,
      "from"            -> encodeFrom(tx.from),
      "to"              -> tx.to.asJson,
      "signatures"      -> tx.attestation.asJson,
      "fee"             -> tx.fee.asJson(Int128Codec.jsonEncoder),
      "timestamp"       -> tx.timestamp.asJson,
      "data"            -> tx.data.asJson,
      "minting"         -> tx.minting.asJson
    ).asJson
  }

  implicit def jsonDecoder(implicit networkPrefix: NetworkPrefix): Decoder[AssetTransfer[_ <: Proposition]] =
    (c: HCursor) =>
      for {
        from      <- c.downField("from").as[IndexedSeq[(Address, Box.Nonce)]]
        to        <- c.downField("to").as[IndexedSeq[(Address, TokenValueHolder)]]
        fee       <- c.get[Int128]("fee")(Int128Codec.jsonDecoder)
        timestamp <- c.downField("timestamp").as[Long]
        data      <- c.downField("data").as[Option[Latin1Data]]
        minting   <- c.downField("minting").as[Boolean]
        propType  <- c.downField("propositionType").as[String]
      } yield (propType match {
        case PublicKeyPropositionCurve25519.`typeString` =>
          c.downField("signatures").as[ListMap[PublicKeyPropositionCurve25519, SignatureCurve25519]].map {
            new AssetTransfer[PublicKeyPropositionCurve25519](from, to, _, fee, timestamp, data, minting)
          }

        case ThresholdPropositionCurve25519.`typeString` =>
          c.downField("signatures").as[ListMap[ThresholdPropositionCurve25519, ThresholdSignatureCurve25519]].map {
            new AssetTransfer[ThresholdPropositionCurve25519](from, to, _, fee, timestamp, data, minting)
          }

        case PublicKeyPropositionEd25519.`typeString` =>
          c.downField("signatures").as[ListMap[PublicKeyPropositionEd25519, SignatureEd25519]].map {
            new AssetTransfer[PublicKeyPropositionEd25519](from, to, _, fee, timestamp, data, minting)
          }
      }) match {
        case Right(tx) => tx
        case Left(ex)  => throw ex
      }
}
