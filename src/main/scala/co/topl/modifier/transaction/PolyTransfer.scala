package co.topl.modifier.transaction

import java.time.Instant
import co.topl.attestation._
import co.topl.modifier.transaction.Transaction.TxType
import co.topl.modifier.transaction.TransferTransaction.BoxParams
import co.topl.nodeView.state.StateReader
import co.topl.nodeView.state.box.{Box, PolyBox, SimpleValue, TokenBox, TokenValueHolder}
import co.topl.utils.HasName
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}

import scala.util.Try

case class PolyTransfer[
  P <: Proposition: EvidenceProducer: HasName
] (override val from       : IndexedSeq[(Address, Box.Nonce)],
   override val to         : IndexedSeq[(Address, TokenValueHolder)],
   override val attestation: Map[P, Proof[P]],
   override val fee        : Long,
   override val timestamp  : Long,
   override val data       : String,
   override val minting    : Boolean = false
  ) extends TransferTransaction[TokenValueHolder, P](from, to, attestation, fee, timestamp, data, minting) {

  override val txTypePrefix: TxType = PolyTransfer.txTypePrefix

  override lazy val newBoxes: Traversable[TokenBox[SimpleValue]] = {
    val params = TransferTransaction.boxParams(this)

    val feeBox =
      if (fee > 0L) Traversable((PolyBox.apply _).tupled(BoxParams.unapply(params._1).get))
      else Traversable()

    val polyBoxes = params._2.map {
      case BoxParams(ev, n, v: SimpleValue) => PolyBox(ev, n, v)
      case _ => throw new Error("Attempted application of invalid value holder")
    }

    feeBox ++ polyBoxes
  }
}

object PolyTransfer {
  val txTypePrefix: TxType = 2: Byte
  val txTypeString: String = "PolyTransfer"

  implicit val name: HasName[PolyTransfer[_]] = HasName.instance { () => txTypeString }

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
    P <: Proposition: EvidenceProducer: HasName
  ] (stateReader  : StateReader,
     toReceive    : IndexedSeq[(Address, SimpleValue)],
     sender       : IndexedSeq[Address],
     changeAddress: Address,
     fee          : Long,
     data         : String
    ): Try[PolyTransfer[P]] =
    TransferTransaction.createRawTransferParams(stateReader, toReceive, sender, changeAddress, fee, "PolyTransfer").map {
      case (inputs, outputs) => PolyTransfer[P](inputs, outputs, Map(), fee, Instant.now.toEpochMilli, data)
    }

  implicit def jsonEncoder[P <: Proposition]: Encoder[PolyTransfer[P]] = {
    tx: PolyTransfer[P] =>
      Map(
        "txId" -> tx.id.asJson,
        "txType" -> "PolyTransfer".asJson,
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

  implicit def jsonDecoder: Decoder[PolyTransfer[_ <: Proposition]] = (c: HCursor) =>
    for {
      from <- c.downField("from").as[IndexedSeq[(Address, Box.Nonce)]]
      to <- c.downField("to").as[IndexedSeq[(Address, SimpleValue)]]
      fee <- c.downField("fee").as[Long]
      timestamp <- c.downField("timestamp").as[Long]
      data <- c.downField("data").as[String]
      propType <- c.downField("propositionType").as[String]
    } yield {
      (propType match {
        case PublicKeyPropositionCurve25519.typeString =>
          c.downField("signatures").as[Map[PublicKeyPropositionCurve25519, SignatureCurve25519]].map {
            new PolyTransfer[PublicKeyPropositionCurve25519](from, to, _, fee, timestamp, data)
          }

        case ThresholdPropositionCurve25519.typeString =>
          c.downField("signatures").as[Map[ThresholdPropositionCurve25519, ThresholdSignatureCurve25519]].map {
            new PolyTransfer[ThresholdPropositionCurve25519](from, to, _, fee, timestamp, data)
          }
      }) match {
        case Right(tx) => tx
        case Left(ex)  => throw ex
      }
    }
}