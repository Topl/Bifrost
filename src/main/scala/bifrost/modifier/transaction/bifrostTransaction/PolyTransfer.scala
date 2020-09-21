package bifrost.modifier.transaction.bifrostTransaction

import java.time.Instant

import bifrost.crypto.{ FastCryptographicHash, PrivateKey25519, Signature25519 }
import bifrost.modifier.box.proposition.{ ProofOfKnowledgeProposition, PublicKey25519Proposition }
import bifrost.modifier.box.{ Box, PolyBox }
import bifrost.modifier.transaction.bifrostTransaction.Transaction.{ Nonce, Value }
import bifrost.modifier.transaction.serialization.PolyTransferSerializer
import bifrost.state.{ State, StateReader, TokenBoxRegistry }
import bifrost.utils.serialization.BifrostSerializer
import bifrost.wallet.Wallet
import com.google.common.primitives.Ints
import io.circe.Json

import scala.util.{ Failure, Success, Try }

case class PolyTransfer ( override val from      : IndexedSeq[(PublicKey25519Proposition, Nonce)],
                          override val to        : IndexedSeq[(PublicKey25519Proposition, Long)],
                          override val signatures: Map[PublicKey25519Proposition, Signature25519],
                          override val fee       : Long,
                          override val timestamp : Long,
                          override val data      : String
                        )
  extends TransferTransaction(from, to, signatures, fee, timestamp, data) {

  override type M = PolyTransfer

  override lazy val serializer: BifrostSerializer[M] = PolyTransferSerializer

  override lazy val messageToSign: Array[Byte] = "PolyTransfer".getBytes() ++ super.commonMessageToSign

  override lazy val json: Json = super.json("PolyTransfer")

  override lazy val newBoxes: Traversable[PolyBox] =
    to.filter(toInstance => toInstance._2 > 0L)
      .zipWithIndex
      .map {
        case ((prop, value), idx) =>
          val nonce = PolyTransfer
            .nonceFromDigest(FastCryptographicHash("PolyTransfer".getBytes
                                                     ++ prop.pubKeyBytes
                                                     ++ hashNoNonces
                                                     ++ Ints.toByteArray(idx)))

          PolyBox(prop, nonce, value)
      }

  override def toString: String = s"PolyTransfer(${json.noSpaces})"

}

object PolyTransfer extends TransferUtil {

  type SR = StateReader[Box, ProofOfKnowledgeProposition[PrivateKey25519], Any]

  def create ( tbr      : TokenBoxRegistry,
               w        : Wallet,
               toReceive: IndexedSeq[(PublicKey25519Proposition, Long)],
               sender   : IndexedSeq[PublicKey25519Proposition],
               fee      : Long, data: String
             ): Try[PolyTransfer] = Try {
    val params = parametersForCreate(tbr, w, toReceive, sender, fee, "PolyTransfer")
    val timestamp = Instant.now.toEpochMilli
    PolyTransfer(params._1.map(t => t._1 -> t._2), params._2, fee, timestamp, data)
  }

  def apply ( from     : IndexedSeq[(PrivateKey25519, Nonce)],
              to       : IndexedSeq[(PublicKey25519Proposition, Value)],
              fee      : Long,
              timestamp: Long,
              data     : String
            ): PolyTransfer = {
    val params = parametersForApply(from, to, fee, timestamp, "PolyTransfer", data).get
    PolyTransfer(params._1, to, params._2, fee, timestamp, data)
  }

  def createPrototype ( tbr      : TokenBoxRegistry,
                        toReceive: IndexedSeq[(PublicKey25519Proposition, Long)],
                        sender   : IndexedSeq[PublicKey25519Proposition],
                        fee      : Long,
                        data     : String
                      ): Try[PolyTransfer] = Try {
    val params = parametersForCreate(tbr, toReceive, sender, fee, "PolyTransfer")
    val timestamp = Instant.now.toEpochMilli
    PolyTransfer(params._1.map(t => t._1 -> t._2), params._2, Map(), fee, timestamp, data)
  }

  def validatePrototype(tx: PolyTransfer): Try[Unit] = validateTransfer(tx, withSigs = false)

  def syntacticValidate(tx: PolyTransfer): Try[Unit] = validateTransfer(tx)

  def semanticValidate(tx: PolyTransfer, state: SR): Try[Unit] = {

    // check that the transaction is correctly formed before checking state
    syntacticValidate(tx) match {
      case Failure(e) => throw e
      case _ => // continue processing
    }

    // compute transaction values used for validation
    val txOutput = tx.newBoxes.map(b => b.value).sum
    val unlockers = State.generateUnlockers(tx.from, tx.signatures)

    // iterate through the unlockers and sum up the value of the box for each valid unlocker
    unlockers.foldLeft[Try[Long]](Success(0L))((trySum, unlocker) => {
      trySum.flatMap(partialSum =>
                       state.closedBox(unlocker.closedBoxId) match {
                         case Some(box: PolyBox) if unlocker.boxKey.isValid(box.proposition, tx.messageToSign) =>
                           Success(partialSum + box.value)
                         case Some(_) => Failure(new Exception("Invalid unlocker"))
                         case None    => Failure(new Exception(s"Box for unlocker $unlocker cannot be found in state"))
                         case _       => Failure(new Exception("Invalid Box type for this transaction"))
                       }
                     )
    }) match {
      case Success(sum: Long) if txOutput == sum - tx.fee => Success(Unit)
      case Success(sum: Long) => Failure(new Exception(s"Tx output value not equal to input value. $txOutput != ${sum - tx.fee}"))
      case Failure(e)         => throw e
    }
  }

}