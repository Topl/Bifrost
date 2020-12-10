package co.topl.modifier.transaction

import java.time.Instant
import co.topl.attestation._
import co.topl.modifier.transaction.Transaction.TxType
import co.topl.modifier.transaction.TransferTransaction.BoxParams
import co.topl.nodeView.state.StateReader
import co.topl.nodeView.state.box.{AssetBox, AssetCode, AssetValue, Box, PolyBox, TokenBox, TokenValueHolder}
import co.topl.utils.HasName
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}

import scala.util.Try

case class AssetTransfer[
  P <: Proposition: EvidenceProducer: HasName
] (override val from       : IndexedSeq[(Address, Box.Nonce)],
   override val to         : IndexedSeq[(Address, TokenValueHolder)],
   override val attestation: Map[P, Proof[P]],
   override val fee        : Long,
   override val timestamp  : Long,
   override val data       : String,
   override val minting    : Boolean
  ) extends TransferTransaction[TokenValueHolder, P](from, to, attestation, fee, timestamp, data, minting) {

  override val txTypePrefix: TxType = AssetTransfer.txTypePrefix

  override lazy val newBoxes: Traversable[TokenBox[TokenValueHolder]] = {
    val params = TransferTransaction.boxParams(this)

    val feeBox =
      if (fee > 0L) Traversable((PolyBox.apply _).tupled(BoxParams.unapply(params._1).get))
      else Traversable()

    feeBox ++ params._2.map(p => (AssetBox.apply _).tupled(BoxParams.unapply(p).get))
  }
}

object AssetTransfer {
  val txTypePrefix: TxType = 3: Byte
  val txTypeString: String = "AssetTransfer"

  implicit val name: HasName[AssetTransfer[_]] = HasName.instance { () => txTypeString }

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
     toReceive    : IndexedSeq[(Address, AssetValue)],
     sender       : IndexedSeq[Address],
     changeAddress: Address,
     fee          : Long,
     data         : String,
     minting      : Boolean
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
        fee,
        "AssetTransfer",
        Some((assetCode, minting))
      ).map { case (inputs, outputs) =>
        AssetTransfer[P](inputs, outputs, Map(), fee, Instant.now.toEpochMilli, data, minting)
    }
  }

  implicit def jsonEncoder[P <: Proposition]: Encoder[AssetTransfer[P]] = {
    tx: AssetTransfer[P] =>
      Map(
        "txId" -> tx.id.asJson,
        "txType" -> "AssetTransfer".asJson,
        "propositionType" -> tx.getPropTypeString.asJson,
        "newBoxes" -> tx.newBoxes.toSeq.asJson,
        "boxesToRemove" -> tx.boxIdsToOpen.asJson,
        "from" -> tx.from.asJson,
        "to" -> tx.to.asJson,
        "signatures" -> tx.attestation.asJson,
        "fee" -> tx.fee.asJson,
        "timestamp" -> tx.timestamp.asJson,
        "data" -> tx.data.asJson,
      ).asJson
  }

  implicit def jsonDecoder: Decoder[AssetTransfer[_ <: Proposition]] =
    (c: HCursor) =>
      for {
        from <- c.downField("from").as[IndexedSeq[(Address, Long)]]
        to <- c.downField("to").as[IndexedSeq[(Address, TokenValueHolder)]]
        fee <- c.downField("fee").as[Long]
        timestamp <- c.downField("timestamp").as[Long]
        data <- c.downField("data").as[String]
        issuer <- c.downField("issuer").as[Address]
        assetCode <- c.downField("assetCode").as[String]
        minting <- c.downField("minting").as[Boolean]
        propType <- c.downField("propositionType").as[String]
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