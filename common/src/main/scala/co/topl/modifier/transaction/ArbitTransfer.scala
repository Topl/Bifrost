package co.topl.modifier.transaction

import co.topl.attestation._
import co.topl.modifier.box._
import co.topl.modifier.transaction.Transaction.TxType
import co.topl.modifier.transaction.TransferTransaction.{encodeFrom, BoxParams}
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.StringDataTypes.Latin1Data
import co.topl.utils.codecs.json._
import co.topl.utils.{Identifiable, Identifier, Int128}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}

import scala.collection.immutable.ListMap

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
      "fee"             -> tx.fee.asJson,
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
        fee       <- c.get[Int128]("fee")
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
