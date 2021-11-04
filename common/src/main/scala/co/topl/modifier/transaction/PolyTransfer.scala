package co.topl.modifier.transaction

import co.topl.attestation._
import co.topl.modifier.box._
import co.topl.modifier.transaction.Transaction.TxType
import co.topl.modifier.transaction.TransferTransaction.{encodeFrom, BoxParams}
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.StringDataTypes.Latin1Data
import co.topl.utils.codecs.json._
import co.topl.utils.{Identifiable, Identifier, Int128}
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}

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
      "fee"             -> tx.fee.asJson,
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
        fee       <- c.get[Int128]("fee")
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
