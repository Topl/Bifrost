package co.topl.modifier.transaction

import java.time.Instant

import co.topl.attestation
import co.topl.attestation.proof.{Proof, SignatureCurve25519, ThresholdSignatureCurve25519}
import co.topl.attestation.proposition.{Proposition, PublicKeyPropositionCurve25519, ThresholdPropositionCurve25519}
import co.topl.attestation.{Address, EvidenceProducer}
import co.topl.modifier.transaction.Transaction.TxType
import co.topl.nodeView.state.StateReader
import co.topl.nodeView.state.box.{ArbitBox, Box, PolyBox, TokenBox}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}

import scala.util.Try

case class ArbitTransfer[
  P <: Proposition: EvidenceProducer
] (override val from       : IndexedSeq[(Address, Box.Nonce)],
   override val to         : IndexedSeq[(Address, TokenBox.Value)],
   override val attestation: Map[P, Proof[P]],
   override val fee        : Long,
   override val timestamp  : Long,
   override val data       : String,
   override val minting    : Boolean = false
  ) extends TransferTransaction[P](from, to, attestation, fee, timestamp, data, minting) {

  override val txTypePrefix: TxType = ArbitTransfer.txTypePrefix

  override lazy val newBoxes: Traversable[TokenBox] = {
    val params = TransferTransaction.boxParams(this)
    Traversable((PolyBox.apply _).tupled(params.head)) ++ params.tail.map((ArbitBox.apply _).tupled(_))
  }
}

object ArbitTransfer {
  val txTypePrefix: TxType = 1: Byte

  /**
   *
   * @param stateReader
   * @param toReceive
   * @param sender
   * @param fee
   * @param data
   * @return
   */
  def createRaw[
    P <: Proposition: EvidenceProducer,
    PR <: Proof[P]
  ] (stateReader  : StateReader,
     toReceive    : IndexedSeq[(Address, TokenBox.Value)],
     sender       : IndexedSeq[Address],
     changeAddress: Address,
     fee          : Long,
     data         : String
    ): Try[ArbitTransfer[P]] =
    TransferTransaction.createRawTransferParams(stateReader, toReceive, sender, changeAddress, fee, "ArbitTransfer").map {
      case (inputs, outputs) => ArbitTransfer[P](inputs, outputs, Map(), fee, Instant.now.toEpochMilli, data)
    }

  implicit def jsonEncoder[P <: Proposition]: Encoder[ArbitTransfer[P]] = {
    tx: ArbitTransfer[P] =>
      Map(
        "txId" -> tx.id.asJson,
        "txType" -> "ArbitTransfer".asJson,
        "propositionType" -> tx.getPropTypeString.asJson,
        "newBoxes" -> tx.newBoxes.toSeq.asJson,
        "boxesToRemove" -> tx.boxIdsToOpen.asJson,
        "from" -> tx.from.asJson,
        "to" -> tx.to.asJson,
        "signatures" -> tx.attestation.asJson,
        "fee" -> tx.fee.asJson,
        "timestamp" -> tx.timestamp.asJson,
        "minting" -> tx.minting.asJson,
        "data" -> tx.data.asJson
      ).asJson
  }

  def jsonDecoder: Decoder[ArbitTransfer[Proposition]] =
    (c: HCursor) => c.downField("propositionType").as[String].flatMap { b =>
      case PublicKeyPropositionCurve25519.typeString =>  jsonDecoderPubKey(c)
      case ThresholdPropositionCurve25519.typeString => ???
    }


  implicit def jsonDecoderPubKey: Decoder[ArbitTransfer[PublicKeyPropositionCurve25519]] =
    (c: HCursor) =>
      for {
        from <- c.downField("from").as[IndexedSeq[(Address, Box.Nonce)]]
        to <- c.downField("to").as[IndexedSeq[(Address, TokenBox.Value)]]
        fee <- c.downField("fee").as[Long]
        timestamp <- c.downField("timestamp").as[Long]
        data <- c.downField("data").as[String]
        sigs <- c.downField("signatures").as[Map[PublicKeyPropositionCurve25519, SignatureCurve25519]]
      } yield {
        new ArbitTransfer[PublicKeyPropositionCurve25519](from, to, sigs, fee, timestamp, data)
      }
}
