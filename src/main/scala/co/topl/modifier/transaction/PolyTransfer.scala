package co.topl.modifier.transaction

import java.time.Instant

import co.topl.attestation
import co.topl.attestation.{Address, EvidenceProducer}
import co.topl.attestation.proof.{Proof, SignatureCurve25519}
import co.topl.attestation.proposition.{Proposition, PublicKeyPropositionCurve25519}
import co.topl.attestation.secrets.PrivateKeyCurve25519
import co.topl.modifier.transaction
import co.topl.modifier.transaction.Transaction.TxType
import co.topl.nodeView.state.StateReader
import co.topl.nodeView.state.box.{Box, PolyBox, TokenBox}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}

import scala.util.{Failure, Success, Try}

case class PolyTransfer[
  P <: Proposition,
  PR <: Proof[P]
] (override val from       : IndexedSeq[(Address, Box.Nonce)],
   override val to         : IndexedSeq[(Address, TokenBox.Value)],
   override val attestation: Map[P, PR],
   override val fee        : Long,
   override val timestamp  : Long,
   override val data       : String,
   override val minting    : Boolean = false
  ) extends TransferTransaction[P, PR](from, to, attestation, fee, timestamp, data, minting) {

  override val txTypePrefix: TxType = PolyTransfer.txTypePrefix

  override lazy val newBoxes: Traversable[TokenBox] = {
    TransferTransaction.boxParams(this).map((PolyBox.apply _).tupled(_))
  }
}

object PolyTransfer {
  val txTypePrefix: TxType = 2: Byte

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
    P <: Proposition,
    PR <: Proof[P]
  ] (stateReader  : StateReader,
     toReceive    : IndexedSeq[(Address, TokenBox.Value)],
     sender       : IndexedSeq[Address],
     changeAddress: Address,
     fee          : Long,
     data         : String
    ): Try[PolyTransfer[P, PR]] =
    TransferTransaction.createRawTransferParams(stateReader, toReceive, sender, changeAddress, fee, "PolyTransfer").map {
      case (inputs, outputs) => PolyTransfer[P, PR](inputs, outputs, Map(), fee, Instant.now.toEpochMilli, data)
    }

  implicit def jsonEncoder[P <: Proposition, PR <: Proof[P]]: Encoder[PolyTransfer[P, PR]] = {
    tx: PolyTransfer[P, PR] =>
      Map(
        "txId" -> tx.id.asJson,
        "txType" -> "PolyTransfer".asJson,
        "propositionType" -> tx.getPropTypeString.asJson,
        "newBoxes" -> tx.newBoxes.toSeq.asJson,
        "boxesToRemove" -> tx.boxIdsToOpen.asJson,
        "from" -> tx.from.asJson,
        "to" -> tx.to.asJson,
        "signatures" -> attestation.jsonEncoder(tx.attestation),
        "fee" -> tx.fee.asJson,
        "timestamp" -> tx.timestamp.asJson,
        "minting" -> tx.minting.asJson,
        "data" -> tx.data.asJson
      ).asJson
  }

  implicit def jsonDecoder: Decoder[PolyTransfer[_ <: Proposition, _ <: Proof[_]]] = (c: HCursor) =>
    for {
      from <- c.downField("from").as[IndexedSeq[(Address, Box.Nonce)]]
      to <- c.downField("to").as[IndexedSeq[(Address, TokenBox.Value)]]
      fee <- c.downField("fee").as[Long]
      timestamp <- c.downField("timestamp").as[Long]
      data <- c.downField("data").as[String]
      attType <- c.downField("propositionType").as[String]
      signatures <- attestation.jsonDecoder(attType, c.downField("signatures"))
    } yield {
      new PolyTransfer(from, to, signatures, fee, timestamp, data)
    }
}