package co.topl.modifier.transaction

import java.time.Instant
import co.topl.attestation._
import co.topl.modifier.transaction.Transaction.TxType
import co.topl.modifier.transaction.TransferTransaction.BoxParams
import co.topl.nodeView.state.StateReader
import co.topl.nodeView.state.box.{AssetBox, AssetCode, AssetValue, Box, PolyBox, TokenBox, TokenValueHolder}
import co.topl.utils.Identifiable
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}

import scala.util.Try

case class AssetTransfer[
  P <: Proposition: EvidenceProducer: Identifiable
](override val from:        IndexedSeq[(Address, Box.Nonce)],
  override val to:          IndexedSeq[(Address, TokenValueHolder)],
  override val attestation: Map[P, Proof[P]],
  override val fee:         Long,
  override val timestamp:   Long,
  override val data:        Option[String] = None,
  override val minting:     Boolean = false
) extends TransferTransaction[TokenValueHolder, P](from, to, attestation, fee, timestamp, data, minting) {

  override val txTypePrefix: TxType = AssetTransfer.txTypePrefix

  override lazy val newBoxes: Traversable[TokenBox[TokenValueHolder]] = {
    val params = TransferTransaction.boxParams(this)

    val feeChangeBox =
      if (fee > 0L) Traversable(PolyBox(params._1.evidence, params._1.nonce, params._1.value))
      else Traversable()

    val assetBoxes = params._2.map {
      case BoxParams(ev, n, v: AssetValue) => AssetBox(ev, n, v)
      case _                               => throw new Error("Attempted application of invalid value holder")
    }

    feeChangeBox ++ assetBoxes
  }
}

object AssetTransfer {
  val txTypePrefix: TxType = 3: Byte
  val txTypeString: String = "AssetTransfer"

  implicit val identifier: Identifiable[AssetTransfer[_]] = new Identifiable[AssetTransfer[_]] {
    override def typePrefix: Byte = txTypePrefix
    override def typeString: String = txTypeString
  }

  /** @param stateReader
    * @param toReceive
    * @param sender
    * @param fee
    * @param data
    * @return
    */
  def createRaw[
    P <: Proposition: EvidenceProducer: Identifiable
  ](stateReader:          StateReader,
    toReceive:            IndexedSeq[(Address, AssetValue)],
    sender:               IndexedSeq[Address],
    changeAddress:        Address,
    consolidationAddress: Option[Address],
    fee:                  Long,
    data:                 Option[String],
    minting:              Boolean
  ): Try[AssetTransfer[P]] = {

    val assetCode =
      toReceive
        .map(_._2.assetCode)
        .toSet
        .ensuring(_.size == 1, s"Found multiple asset codes when only one was expected")
        .head

    TransferTransaction
      .createRawTransferParams(
        stateReader,
        toReceive,
        sender,
        changeAddress,
        consolidationAddress,
        fee,
        "AssetTransfer",
        Some((assetCode, minting))
      )
      .map { case (inputs, outputs) =>
        AssetTransfer[P](inputs, outputs, Map(), fee, Instant.now.toEpochMilli, data, minting)
      }
  }

  implicit def jsonEncoder[P <: Proposition]: Encoder[AssetTransfer[P]] = { tx: AssetTransfer[P] =>
    Map(
      "txId"            -> tx.id.asJson,
      "txType"          -> "AssetTransfer".asJson,
      "propositionType" -> tx.getPropTypeString.asJson,
      "newBoxes"        -> tx.newBoxes.toSeq.asJson,
      "boxesToRemove"   -> tx.boxIdsToOpen.asJson,
      "from"            -> tx.from.asJson,
      "to"              -> tx.to.asJson,
      "signatures"      -> tx.attestation.asJson,
      "fee"             -> tx.fee.asJson,
      "timestamp"       -> tx.timestamp.asJson,
      "data"            -> tx.data.asJson
    ).asJson
  }

  implicit def jsonDecoder: Decoder[AssetTransfer[_ <: Proposition]] =
    (c: HCursor) =>
      for {
        from      <- c.downField("from").as[IndexedSeq[(Address, Long)]]
        to        <- c.downField("to").as[IndexedSeq[(Address, TokenValueHolder)]]
        fee       <- c.downField("fee").as[Long]
        timestamp <- c.downField("timestamp").as[Long]
        data      <- c.downField("data").as[Option[String]]
        minting   <- c.downField("minting").as[Boolean]
        propType  <- c.downField("propositionType").as[String]
      } yield {
        (propType match {
          case PublicKeyPropositionCurve25519.typeString =>
            c.downField("signatures").as[Map[PublicKeyPropositionCurve25519, SignatureCurve25519]].map {
              new AssetTransfer[PublicKeyPropositionCurve25519](from, to, _, fee, timestamp, data, minting)
            }

          case ThresholdPropositionCurve25519.typeString =>
            c.downField("signatures").as[Map[ThresholdPropositionCurve25519, ThresholdSignatureCurve25519]].map {
              new AssetTransfer[ThresholdPropositionCurve25519](from, to, _, fee, timestamp, data, minting)
            }
        }) match {
          case Right(tx) => tx
          case Left(ex)  => throw ex
        }
      }
}
