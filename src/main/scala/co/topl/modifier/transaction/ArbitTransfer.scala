package co.topl.modifier.transaction

import java.time.Instant

import co.topl.crypto.{FastCryptographicHash, PrivateKey25519, Signature25519}
import co.topl.modifier.transaction.Transaction.{Nonce, Value}
import co.topl.modifier.transaction.serialization.ArbitTransferSerializer
import co.topl.nodeView.state.box.ArbitBox
import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
import co.topl.nodeView.state.{State, TokenBoxRegistry}
import co.topl.utils.serialization.BifrostSerializer
import com.google.common.primitives.Ints
import io.circe.{Decoder, Encoder, HCursor, Json}

import scala.util.{Failure, Success, Try}

case class ArbitTransfer ( override val from      : IndexedSeq[(PublicKey25519Proposition, Nonce)],
                           override val to        : IndexedSeq[(PublicKey25519Proposition, Long)],
                           override val signatures: Map[PublicKey25519Proposition, Signature25519],
                           override val fee       : Long,
                           override val timestamp : Long,
                           override val data      : String
                         ) extends TransferTransaction(from, to, signatures, fee, timestamp, data) {

  override type M = ArbitTransfer

  override lazy val serializer: BifrostSerializer[M] = ArbitTransferSerializer

  override lazy val messageToSign: Array[Byte] = "ArbitTransfer".getBytes ++ super.commonMessageToSign

  override lazy val json: Json = ArbitTransfer.jsonEncoder(this)

  override lazy val newBoxes: Traversable[ArbitBox] =
    to.filter(toInstance => toInstance._2 > 0L)
      .zipWithIndex
      .map { case ((prop, value), idx) =>
        val nonce = Transaction
          .nonceFromDigest(
            FastCryptographicHash(
              "ArbitTransfer".getBytes
                ++ prop.pubKeyBytes
                ++ hashNoNonces
                ++ Ints.toByteArray(idx)))

        ArbitBox(prop, nonce, value)
      }

  override def toString: String = s"ArbitTransfer(${json.noSpaces})"
}

//noinspection ScalaStyle
object ArbitTransfer extends TransferUtil {

  /**
    *
    * @param from
    * @param to
    * @param fee
    * @param timestamp
    * @param data
    * @return
    */
  def apply (from: IndexedSeq[(PrivateKey25519, Nonce)],
             to: IndexedSeq[(PublicKey25519Proposition, Value)],
             fee       : Long,
             timestamp : Long,
             data      : String
            ): ArbitTransfer = {
    val params = parametersForApply(from, to, fee, timestamp, "ArbitTransfer", data).get
    new ArbitTransfer(params._1, to, params._2, fee, timestamp, data)
  }

  //  /**
  //    *
  //    * @param tbr
  //    * @param stateReader
  //    * @param w
  //    * @param toReceive
  //    * @param sender
  //    * @param fee
  //    * @param data
  //    * @return
  //    */
  //  def create ( tbr: TokenBoxRegistry,
  //               stateReader: SR,
  //               w: Wallet,
  //               toReceive: IndexedSeq[(PublicKey25519Proposition, Long)],
  //               sender: IndexedSeq[PublicKey25519Proposition], fee: Long, data: String
  //             ): Try[ArbitTransfer] = Try {
  //
  //    val params = parametersForCreate(tbr, stateReader, w, toReceive, sender, fee, "ArbitTransfer")
  //    val timestamp = Instant.now.toEpochMilli
  //    ArbitTransfer(params._1.map(t => t._1 -> t._2), params._2, fee, timestamp, data)
  //  }

  /**
    *
    * @param tbr
    * @param stateReader
    * @param toReceive
    * @param sender
    * @param fee
    * @param data
    * @return
    */
  def createPrototype (tbr: TokenBoxRegistry,
                       stateReader: SR,
                       toReceive: IndexedSeq[(PublicKey25519Proposition, Long)],
                       sender: IndexedSeq[PublicKey25519Proposition], fee: Long, data: String
                      ): Try[ArbitTransfer] = Try {
    val params = parametersForCreate(tbr, stateReader, toReceive, sender, fee, "ArbitTransfer")
    val timestamp = Instant.now.toEpochMilli
    ArbitTransfer(params._1.map(t => t._1 -> t._2), params._2, Map(), fee, timestamp, data)
  }

  /**
    *
    * @param tx
    * @return
    */
  def validatePrototype (tx: ArbitTransfer): Try[Unit] = validateTransfer(tx, withSigs = false)

  /**
    *
    * @param tx
    * @param state
    * @return
    */
  def semanticValidate (tx: ArbitTransfer, state: SR): Try[Unit] = {

    // check that the transaction is correctly formed before checking state
    syntacticValidate(tx) match {
      case Failure(e) => throw e
      case _          => // continue processing
    }

    // compute transaction values used for validation
    val txOutput = tx.newBoxes.map(b => b.value).sum
    val unlockers = State.generateUnlockers(tx.from, tx.signatures)

    // iterate through the unlockers and sum up the value of the box for each valid unlocker
    unlockers.foldLeft[Try[Long]](Success(0L))((trySum, unlocker) => {
      trySum.flatMap { partialSum =>
        state.getBox(unlocker.closedBoxId) match {
          case Some(box: ArbitBox) if unlocker.boxKey
            .isValid(box.proposition, tx.messageToSign) => Success(partialSum + box.value)
          case Some(_)                                  => Failure(new Exception("Invalid unlocker"))
          case None                                     => Failure(new Exception(s"Box for unlocker $unlocker cannot be found in state"))
          case _                                        => Failure(new Exception("Invalid Box type for this transaction"))
        }
      }
    }) match {
      case Success(sum: Long) if txOutput == sum - tx.fee => Success(Unit)
      case Success(sum: Long)                             => Failure(new Exception(s"Tx output value not equal to input value. $txOutput != ${
        sum - tx.fee
      }"))
      case Failure(e)                                     => throw e
    }
  }

  /**
    *
    * @param tx
    * @return
    */
  def syntacticValidate (tx: ArbitTransfer): Try[Unit] = validateTransfer(tx)

  implicit val jsonEncoder: Encoder[ArbitTransfer] = { tx: ArbitTransfer =>
    super.jsonEncoder("ArbitTransfer", tx)
  }

  implicit val jsonDecoder: Decoder[ArbitTransfer] = (c: HCursor) =>
    for {
      rawFrom <- c.downField("from").as[IndexedSeq[(String, String)]]
      rawTo <- c.downField("to").as[IndexedSeq[(String, String)]]
      rawSignatures <- c.downField("signatures").as[Map[String, String]]
      fee <- c.downField("fee").as[Long]
      timestamp <- c.downField("timestamp").as[Long]
      data <- c.downField("data").as[String]
    } yield {
      val from = rawFrom.map { case (prop, nonce) => PublicKey25519Proposition(prop) -> nonce.toLong }
      val to = rawTo.map { case (prop, value) => PublicKey25519Proposition(prop) -> value.toLong }
      val signatures = rawSignatures.map { case (prop, sig) => PublicKey25519Proposition(prop) -> Signature25519(sig) }

      ArbitTransfer(from, to, signatures, fee, timestamp, data)
    }
}