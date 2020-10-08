package co.topl.modifier.transaction

import co.topl.crypto.{ FastCryptographicHash, Signature25519 }
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.transaction.Transaction.Nonce
import co.topl.nodeView.state.StateReader
import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
import co.topl.nodeView.state.box.{ ArbitBox, Box, BoxId, TokenBox }
import com.google.common.primitives.Longs
import io.circe.syntax._
import io.circe.{ Decoder, Encoder, HCursor }

import scala.util.Try

case class Coinbase ( to        : IndexedSeq[(PublicKey25519Proposition, Long)],
                      signatures: Map[PublicKey25519Proposition, Signature25519],
                      timestamp : Long,
                      parentId  : ModifierId
                    ) extends Transaction {

  lazy val fee = 0L // you don't ever pay for a Coinbase TX since you'd be paying yourself so fee must equal 0

  override lazy val boxIdsToOpen: IndexedSeq[BoxId] = IndexedSeq()

  lazy val hashNoNonces: FastCryptographicHash.Digest = FastCryptographicHash(
    to.head._1.pubKeyBytes ++
      Longs.toByteArray(timestamp) ++
      Longs.toByteArray(fee) ++
      parentId.hashBytes
    )

  val nonce: Nonce = Transaction.nonceFromDigest(FastCryptographicHash(
    "Coinbase".getBytes ++ hashNoNonces
  ))

  lazy val newBoxes: Traversable[TokenBox] =
    if (to.head._2 > 0L) Traversable(ArbitBox(to.head._1, nonce, to.head._2))
    else Traversable[TokenBox]()

  // just tac on the byte string "Coinbase" to the beginning of the common message
  override lazy val messageToSign: Array[Byte] =
      "Coinbase".getBytes() ++
      super.messageToSign ++
      parentId.hashBytes

  override def toString: String = s"Coinbase(${json.noSpaces})"

}

object Coinbase {

  type SR = StateReader[Box]

  implicit val jsonEncoder: Encoder[Coinbase] = (tx: Coinbase) =>
    Map(
      "txHash" -> tx.id.asJson,
      "txType" -> "Coinbase".asJson,
      "parentId" -> tx.parentId.asJson,
      "newBoxes" -> tx.newBoxes.map(_.json).toSeq.asJson,
      "to" -> tx.to.asJson,
      "fee" -> tx.fee.asJson,
      "signatures" -> tx.signatures.asJson,
      "timestamp" -> tx.timestamp.asJson
      ).asJson

  implicit val jsonDecoder: Decoder[Coinbase] = (c: HCursor) =>
    for {
      to <- c.downField("to").as[IndexedSeq[(PublicKey25519Proposition, Long)]]
      signatures <- c.downField("signatures").as[Map[PublicKey25519Proposition, Signature25519]]
      timestamp <- c.downField("timestamp").as[Long]
      parentId <- c.downField("parentId").as[ModifierId]
    } yield {
      Coinbase (to, signatures, timestamp, parentId)
    }

  /**
   * Create a raw (unsigned) coinbase transaction
   *
   * @param rewardAddr the account that will receive the rewards from the coinbase
   * @param amount inflation amount to distribute
   * @param timestamp time of block creation
   * @param parentId id of the block preceding the block including this transaction
   * @return a raw conbase transaction
   */
  def createRaw ( rewardAddr: PublicKey25519Proposition,
                  amount: Long,
                  timestamp: Long,
                  parentId: Block.BlockId
                ): Coinbase = {
    val to = IndexedSeq((rewardAddr, amount))
    val sig = Map(rewardAddr ->  Signature25519(Array.emptyByteArray))
    Coinbase(to, sig, timestamp, parentId)
  }

  /**
   *
   * @param tx
   * @param withSigs
   * @return
   */
  def syntacticValidate( tx: Coinbase, withSigs: Boolean = true): Try[Unit] = Try {
    require(tx.to.head._2 >= 0L) // can't make negative Arbits. anti-Arbits?!?!
    require(tx.fee == 0)
    require(tx.timestamp >= 0)
    require(tx.signatures.forall { signature => // should be only one sig
      signature._2.isValid(tx.to.head._1, tx.messageToSign) // because this is set to self the signer is also the reciever
    }, "Invalid signature")
  }

  /**
   *
   * @param tx
   * @return
   */
  def validatePrototype(tx: Coinbase): Try[Unit] = syntacticValidate(tx, withSigs = false)

  /**
   *
   * @param tx
   * @param state
   * @return
   */
  def semanticValidate( tx: Coinbase, state: SR): Try[Unit] = {
    // check that the transaction is correctly formed before checking state
    syntacticValidate(tx)
  }
}
