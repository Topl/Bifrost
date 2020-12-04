package co.topl.modifier.transaction

import co.topl.attestation._
import co.topl.modifier.transaction.Transaction.TxType
import co.topl.modifier.transaction.TransferTransaction.BoxParams
import co.topl.nodeView.state.StateReader
import co.topl.nodeView.state.box.Box.Nonce
import co.topl.nodeView.state.box._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}

import java.time.Instant
import scala.util.Try

case class GenericTransfer[
  P <: Proposition: EvidenceProducer,
  T <: TokenValueHolder,
  BX <: TokenBox[T]
](override val from:        IndexedSeq[(Address, Box.Nonce)],
  override val to:          IndexedSeq[TokenRecipient[T, BX]],
  override val attestation: Map[P, Proof[P]],
  override val fee:         Long,
  override val timestamp:   Long,
  override val data:        String,
  override val minting:     Boolean = false
) extends TransferTransaction[P, T, BX](from, to, attestation, fee, timestamp, data, minting) {

  lazy val txTypePrefix: TxType = ArbitTransfer.txTypePrefix

  override lazy val newBoxes: Traversable[TokenBox[_ <: TokenValueHolder]] = {
    val params = TransferTransaction.boxParams(this)

    val feeBox =
      if (fee > 0L) Traversable((PolyBox.apply _).tupled(BoxParams.unapply(params._1).get))
      else Traversable()

    feeBox ++ params._2.map(p => (ArbitBox.apply _).tupled(BoxParams.unapply(p).get))
  }
}

object GenericTransfer {
  val txTypePrefix: TxType = 10: Byte

  def check(tx: GenericTransfer[_,_,_]): TxType = tx match {
    case ArbitTransfer.txTypePrefix => "tmp"
  }



  def unapply[
    P <: Proposition: EvidenceProducer,
    T <: TokenValueHolder,
    BX <: TokenBox[T]
  ](tx: GenericTransfer[P, T, BX]): Option[TxType] = tx.newBoxes.tail.headOption.map {
    case _: ArbitBox => ArbitTransfer.txTypePrefix
    case _: PolyBox  => PolyTransfer.txTypePrefix
    case _: AssetBox => AssetTransfer.txTypePrefix
  }

  /** @param stateReader
    * @param toReceive
    * @param sender
    * @param fee
    * @param data
    * @return
    */
  def createRaw[
    P <: Proposition: EvidenceProducer,
    T <: TokenValueHolder,
    BX <: TokenBox[T]
  ](stateReader:   StateReader,
    toReceive:     IndexedSeq[TokenRecipient[T, BX]],
    sender:        IndexedSeq[Address],
    changeAddress: Address,
    fee:           Long,
    data:          String
  ): Try[GenericTransfer[P, T, BX]] =
    TransferTransaction
      .createRawTransferParams(stateReader, toReceive, sender, changeAddress, fee, "ArbitTransfer")
      .map { case (inputs, outputs) =>
        GenericTransfer[P, T, BX](inputs, outputs, Map(), fee, Instant.now.toEpochMilli, data)
      }

  implicit def jsonEncoder[P <: Proposition]: Encoder[GenericTransfer[P]] = { tx: GenericTransfer[P] =>
    Map(
      "txId"            -> tx.id.asJson,
      "txType"          -> "ArbitTransfer".asJson,
      "propositionType" -> tx.getPropTypeString.asJson,
      "newBoxes"        -> tx.newBoxes.toSeq.asJson,
      "boxesToRemove"   -> tx.boxIdsToOpen.asJson,
      "from"            -> tx.from.asJson,
      "to"              -> tx.to.asJson,
      "signatures"      -> tx.attestation.asJson,
      "fee"             -> tx.fee.asJson,
      "timestamp"       -> tx.timestamp.asJson,
      "minting"         -> tx.minting.asJson,
      "data"            -> tx.data.asJson
    ).asJson
  }

  implicit def jsonDecoder: Decoder[GenericTransfer[_ <: Proposition]] =
    (c: HCursor) =>
      for {
        from      <- c.downField("from").as[IndexedSeq[(Address, Box.Nonce)]]
        to        <- c.downField("to").as[IndexedSeq[(Address, TokenBox.Value)]]
        fee       <- c.downField("fee").as[Long]
        timestamp <- c.downField("timestamp").as[Long]
        data      <- c.downField("data").as[String]
        propType  <- c.downField("propositionType").as[String]
      } yield {
        (propType match {
          case PublicKeyPropositionCurve25519.typeString =>
            c.downField("signatures").as[Map[PublicKeyPropositionCurve25519, SignatureCurve25519]].map {
              new GenericTransfer[PublicKeyPropositionCurve25519](from, to, _, fee, timestamp, data)
            }

          case ThresholdPropositionCurve25519.typeString =>
            c.downField("signatures").as[Map[ThresholdPropositionCurve25519, ThresholdSignatureCurve25519]].map {
              new GenericTransfer[ThresholdPropositionCurve25519](from, to, _, fee, timestamp, data)
            }
        }) match {
          case Right(tx) => tx
          case Left(ex)  => throw ex
        }
      }
}
