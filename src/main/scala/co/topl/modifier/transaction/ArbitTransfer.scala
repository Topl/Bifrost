package co.topl.modifier.transaction

import java.time.Instant

import co.topl.attestation
import co.topl.attestation.proof.Proof
import co.topl.attestation.proposition.Proposition
import co.topl.attestation.{Address, BoxUnlocker, EvidenceProducer}
import co.topl.nodeView.state.box.{ArbitBox, Box, TokenBox}
import com.google.common.primitives.Ints
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}
import scorex.crypto.hash.Blake2b256

import scala.util.{Failure, Success, Try}

case class ArbitTransfer[P <: Proposition] (signatures             : Map[P, _ <: Proof[P]],
                                            override val from      : IndexedSeq[(Address, Box.Nonce)],
                                            override val to        : IndexedSeq[(Address, TokenBox.Value)],
                                            override val fee       : Long,
                                            override val timestamp : Long,
                                            override val data      : String
                                           ) extends TransferTransaction[P, _ <: Proof[P]](from, to, signatures, fee, timestamp, data) {

  override lazy val messageToSign: Array[Byte] = "ArbitTransfer".getBytes ++ super.messageToSign

  override lazy val newBoxes: Traversable[ArbitBox] =
    to.filter(_._2 > 0L)
      .zipWithIndex
      .map { case ((addr, value), idx) =>
        val nonce = Transaction
          .nonceFromDigest(
            Blake2b256(
              "ArbitTransfer".getBytes
                ++ addr.bytes
                ++ hashNoNonces
                ++ Ints.toByteArray(idx)))

        ArbitBox(addr.evidence, nonce, value)
      }

  override def toString: String = s"ArbitTransfer(${ArbitTransfer.jsonEncoder(this).noSpaces})"
}

//noinspection ScalaStyle
object ArbitTransfer extends TransferCompanion {

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
  def createRaw[P <: Proposition] (stateReader: SR,
                 toReceive  : IndexedSeq[(Address, Long)],
                 sender     : IndexedSeq[Address],
                 fee        : Long,
                 data       : String
                ): Try[ArbitTransfer[P]] = Try {
    val params = parametersForCreate(stateReader, toReceive, sender, fee, "ArbitTransfer")
    val timestamp = Instant.now.toEpochMilli
    ArbitTransfer(params._1.map(t => t._1 -> t._2), params._2, Map(), fee, timestamp, data)
  }

  /**
   *
   * @param tx
   * @return
   */
  def validatePrototype ( tx: ArbitTransfer[_] ): Try[Unit] = validateTransfer(tx, withSigs = false)

  /**
   *
   * @param tx
   * @param state
   * @return
   */
  def semanticValidate[P <: Proposition: EvidenceProducer] (tx: ArbitTransfer[P], state: SR ): Try[Unit] = {

    // check that the transaction is correctly formed before checking state
    syntacticValidate(tx) match {
      case Failure(e) => throw e
      case _          => // continue processing
    }

    // compute transaction values used for validation
    val txOutput = tx.newBoxes.map(b => b.value).sum
    val unlockers = BoxUnlocker.generate(tx.from, tx.signatures)

    // iterate through the unlockers and sum up the value of the box for each valid unlocker
    unlockers.foldLeft[Try[Long]](Success(0L))(( trySum, unlocker ) => {
      trySum.flatMap { partialSum =>
        state.getBox(unlocker.closedBoxId) match {
          case Some(box: ArbitBox) if unlocker.boxKey.isValid(unlocker.proposition, tx.messageToSign) =>
            Success(partialSum + box.value)

          case Some(_) => Failure(new Exception("Invalid unlocker"))
          case None    => Failure(new Exception(s"Box for unlocker $unlocker cannot be found in state"))
          case _       => Failure(new Exception("Invalid Box type for this transaction"))
        }
      }
    }) match {
      case Success(sum: Long) if txOutput == sum - tx.fee =>
        Success(Unit)

      case Success(sum: Long) =>
        Failure(new Exception(s"Tx output value not equal to input value. $txOutput != ${sum - tx.fee}"))

      case Failure(e) => throw e
    }
  }

  /**
   *
   * @param tx
   * @return
   */
  def syntacticValidate ( tx: ArbitTransfer[_] ): Try[Unit] = validateTransfer(tx)
}


