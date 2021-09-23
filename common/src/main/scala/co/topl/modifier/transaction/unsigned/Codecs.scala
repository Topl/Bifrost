package co.topl.modifier.transaction.unsigned

import co.topl.attestation.{PublicKeyPropositionCurve25519, PublicKeyPropositionEd25519, ThresholdPropositionCurve25519}
import co.topl.modifier.transaction.TransferTransaction.encodeFrom
import co.topl.modifier.transaction.unsigned.Builder.BoxPickingStrategy
import co.topl.modifier.transaction.unsigned.UnsignedTransferTransaction.getIdBytes
import io.circe.{Decoder, Encoder}
import io.circe.syntax._
import co.topl.utils.codecs.implicits._
import io.circe.generic.semiauto._
import io.circe._

object Codecs {

  trait JsonInstances {

    implicit val propositionTypeJsonEncoder: Encoder[PropositionType] = {
      case PropositionTypes.PublicKeyEd25519    => PublicKeyPropositionEd25519.typeString.asJson
      case PropositionTypes.PublicKeyCurve25519 => PublicKeyPropositionCurve25519.typeString.asJson
      case PropositionTypes.ThresholdCurve25519 => ThresholdPropositionCurve25519.typeString.asJson
    }

    implicit val propositionTypeJsonDecoder: Decoder[PropositionType] = ???

    implicit val boxPickingStrategyJsonEncoder: Encoder[BoxPickingStrategy] = deriveEncoder

    implicit val boxPickingStrategyJsonDecoder: Decoder[BoxPickingStrategy] = deriveDecoder

    // this encoder MUST replicate the PolyTransfer/ArbitTransfer/AssetTransfer encoders
    implicit val unsignedTxJsonEncoder: Encoder[UnsignedTransferTransaction] =
      unsignedTx =>
        Map(
          "txId"            -> getIdBytes(unsignedTx).asJson,
          "txType"          -> TokenType.getTypeString(unsignedTx.transferType).asJson,
          "propositionType" -> unsignedTx.propositionType.asJson,
          "newBoxes"        -> unsignedTx.toBoxes.asJson,
          "boxesToRemove"   -> unsignedTx.fromBoxIds.asJson,
          "from"            -> encodeFrom(unsignedTx.from.toIndexedSeq),
          "to"              -> unsignedTx.to.map(x => x.copy(_2 = TokenValue.getTokenValueHolder(x._2))).asJson,
          "signatures"      -> "".asJson,
          "fee"             -> unsignedTx.fee.asJson,
          "timestamp"       -> unsignedTx.timestamp.asJson,
          "minting"         -> unsignedTx.minting.asJson,
          "data"            -> unsignedTx.data.asJson
        ).asJson

    implicit val unsignedTxJsonDecoder: Decoder[UnsignedTransferTransaction] = ???
  }

  trait Implicits extends JsonInstances

  object implicits extends Implicits
}
