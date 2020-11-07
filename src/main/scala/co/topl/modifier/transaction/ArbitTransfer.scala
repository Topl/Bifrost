package co.topl.modifier.transaction

import java.time.Instant

import co.topl.attestation
import co.topl.attestation.Address
import co.topl.attestation.proof.Proof
import co.topl.attestation.proposition.Proposition
import co.topl.nodeView.state.box.{ArbitBox, Box, TokenBox}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}

import scala.util.Try

case class ArbitTransfer[P <: Proposition] (signatures             : Map[P, _ <: Proof[P]],
                                            override val from      : IndexedSeq[(Address, Box.Nonce)],
                                            override val to        : IndexedSeq[(Address, TokenBox.Value)],
                                            override val fee       : Long,
                                            override val timestamp : Long,
                                            override val data      : String
                                           ) extends TransferTransaction[P, _ <: Proof[P]](from, to, signatures, fee, timestamp, data) {

  override val transactionName = "ArbitTransfer"

  override lazy val newBoxes: Traversable[ArbitBox] = {
    TransferTransaction.boxParams(this).map(
    ArbitBox(addr.evidence, nonce, value)
  }


}

//noinspection ScalaStyle
object ArbitTransfer {

  implicit def jsonEncoder[P <: Proposition]: Encoder[ArbitTransfer[P]] = {
    tx: ArbitTransfer[P] =>
      Map(
        "txHash" -> tx.id.asJson,
        "txType" -> "ArbitTransfer".asJson,
        "propositionType" -> Proposition.getPropTypeString(tx).asJson,
        "newBoxes" -> tx.newBoxes.toSeq.asJson,
        "boxesToRemove" -> tx.boxIdsToOpen.asJson,
        "from" -> tx.from.asJson,
        "to" -> tx.to.asJson,
        "signatures" -> attestation.jsonEncoder(tx.signatures),
        "fee" -> tx.fee.asJson,
        "timestamp" -> tx.timestamp.asJson,
        "data" -> tx.data.asJson
      ).asJson
  }

  implicit def jsonDecoder[P <: Proposition]: Decoder[ArbitTransfer[P]] = ( c: HCursor ) =>
    for {
      from <- c.downField("from").as[IndexedSeq[(Address, Box.Nonce)]]
      to <- c.downField("to").as[IndexedSeq[(Address, TokenBox.Value)]]
      fee <- c.downField("fee").as[Long]
      timestamp <- c.downField("timestamp").as[Long]
      data <- c.downField("data").as[String]
      attType <- c.downField("propositionType").as[String]
    } yield {
      val signatures = attestation.jsonDecoder[P, _ <: Proof[P]](attType, c.downField("signatures"))
      new ArbitTransfer[P](signatures, from, to, fee, timestamp, data)
    }

  /**
   *
   * @param stateReader
   * @param toReceive
   * @param sender
   * @param fee
   * @param data
   * @return
   */
  def createRaw[P <: Proposition] (stateReader  : SR,
                                   toReceive    : IndexedSeq[(Address, TokenBox.Value)],
                                   sender       : IndexedSeq[Address],
                                   changeAddress: Address,
                                   fee          : Long,
                                   data         : String
                                  ): Try[ArbitTransfer[P]] =
    createRawTransferTx(stateReader, toReceive, sender, changeAddress, fee, "ArbitTransfer").map {
      case (inputs, outputs) => ArbitTransfer[P](Map(), inputs, outputs, fee, Instant.now.toEpochMilli, data)
    }
}


