package co.topl.modifier.transaction

import java.time.Instant

import co.topl.attestation
import co.topl.attestation.Address
import co.topl.attestation.proof.Proof
import co.topl.attestation.proposition.Proposition
import co.topl.modifier.transaction.Transaction.TxType
import co.topl.nodeView.state.StateReader
import co.topl.nodeView.state.box.{AssetBox, Box, PolyBox, TokenBox}
import com.google.common.primitives.Bytes
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}

import scala.util.Try

case class AssetTransfer[
  P <: Proposition,
  PR <: Proof[P]
] (override val from       : IndexedSeq[(Address, Box.Nonce)],
   override val to         : IndexedSeq[(Address, TokenBox.Value)],
   override val attestation: Map[P, PR],
   issuer                  : Address,
   assetCode               : String,
   override val fee        : Long,
   override val timestamp  : Long,
   override val data       : String,
   override val minting    : Boolean
  ) extends TransferTransaction[P, PR](from, to, attestation, fee, timestamp, data, minting) {

  override val txTypePrefix: TxType = AssetTransfer.txTypePrefix

  override lazy val newBoxes: Traversable[TokenBox] = {
    val params = TransferTransaction.boxParams(this)
    Traversable((PolyBox.apply _).tupled(params.head)) ++
      params.tail.map(p => (AssetBox(p._1, p._2, p._3, assetCode, issuer, data)))
  }

  override lazy val messageToSign: Array[Byte] = Bytes.concat(,
    super.messageToSign,
    issuer.bytes,
    assetCode.getBytes,
    )
}

object AssetTransfer {
  val txTypePrefix: TxType = 3: Byte

  /**
    *
    * @param stateReader
    * @param toReceive
    * @param sender
    * @param fee
    * @param data
    * @return
    */
  def createRaw[P <: Proposition, PR <: Proof[P]] (stateReader  : StateReader[TokenBox],
                                                   toReceive    : IndexedSeq[(Address, TokenBox.Value)],
                                                   sender       : IndexedSeq[Address],
                                                   changeAddress: Address,
                                                   issuer       : Address,
                                                   assetCode    : String,
                                                   fee          : Long,
                                                   data         : String,
                                                   minting      : Boolean
                                                  ): Try[AssetTransfer[P, PR]] =
    TransferTransaction.createRawTransferParams(stateReader, toReceive, sender, changeAddress, fee, "AssetTransfer", Some((issuer, assetCode))).map {
      case (inputs, outputs) => AssetTransfer[P, PR](inputs, outputs, Map(), issuer, assetCode, fee, Instant.now.toEpochMilli, data, minting)
    }

  implicit def jsonEncoder[P <: Proposition, PR <: Proof[P]]: Encoder[AssetTransfer[P, PR]] = { tx: AssetTransfer[P, PR] =>
    Map(
      "txId" -> tx.id.asJson,
      "txType" -> "AssetTransfer".asJson,
      "propositionType" -> Proposition.getPropTypeString(tx).asJson,
      "newBoxes" -> tx.newBoxes.toSeq.asJson,
      "boxesToRemove" -> tx.boxIdsToOpen.asJson,
      "from" -> tx.from.asJson,
      "to" -> tx.to.asJson,
      "signatures" -> attestation.jsonEncoder(tx.attestation),
      "fee" -> tx.fee.asJson,
      "timestamp" -> tx.timestamp.asJson,
      "data" -> tx.data.asJson,
      "issuer" -> tx.issuer.toString.asJson,
      "assetCode" -> tx.assetCode.asJson,
      ).asJson
  }

  implicit def jsonDecoder[P <: Proposition, PR <: Proof[P]]: Decoder[AssetTransfer[P, PR]] = ( c: HCursor ) =>
    for {
      from <- c.downField("from").as[IndexedSeq[(Address, Long)]]
      to <- c.downField("to").as[IndexedSeq[(Address, Long)]]
      fee <- c.downField("fee").as[Long]
      timestamp <- c.downField("timestamp").as[Long]
      data <- c.downField("data").as[String]
      issuer <- c.downField("issuer").as[Address]
      assetCode <- c.downField("assetCode").as[String]
      minting <- c.downField("minting").as[Boolean]
      attType <- c.downField("propositionType").as[String]
    } yield {
      val signatures = attestation.jsonDecoder[P, PR](attType, c.downField("signatures"))
      AssetTransfer(from, to, signatures, issuer, assetCode, fee, timestamp, data, minting)
    }
}