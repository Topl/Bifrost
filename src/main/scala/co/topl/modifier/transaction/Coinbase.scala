package co.topl.modifier.transaction

import java.time.Instant

import co.topl.crypto.{ FastCryptographicHash, PrivateKey25519, Signature25519 }
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.transaction.Transaction.Nonce
import co.topl.modifier.transaction.serialization.CoinbaseSerializer
import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
import co.topl.nodeView.state.box.{ ArbitBox, Box, TokenBox }
import co.topl.nodeView.state.StateReader
import co.topl.utils.serialization.BifrostSerializer
import com.google.common.primitives.{ Bytes, Longs }
import io.circe.Json
import io.circe.syntax._
import scorex.crypto.encode.Base58

import scala.util.Try

case class Coinbase ( to        : IndexedSeq[(PublicKey25519Proposition, Long)],
                      signatures: Map[PublicKey25519Proposition, Signature25519],
                      timestamp : Long,
                      parentId  : ModifierId
                    ) extends Transaction {
  override type M = Coinbase

  lazy val serializer: BifrostSerializer[Coinbase] = CoinbaseSerializer

  lazy val fee = 0L // you don't ever pay for a Coinbase TX since you'd be paying yourself so fee must equal 0

  override lazy val boxIdsToOpen: IndexedSeq[Array[Byte]] = IndexedSeq()

  lazy val hashNoNonces: FastCryptographicHash.Digest = FastCryptographicHash(
    to.head._1.pubKeyBytes ++
      Longs.toByteArray(timestamp) ++
      Longs.toByteArray(fee) ++
      parentId.hashBytes
    )

  val nonce: Nonce = Coinbase.nonceFromDigest(FastCryptographicHash(
    "Coinbase".getBytes ++ hashNoNonces
  ))

  lazy val newBoxes: Traversable[TokenBox] =
    if (to.head._2 > 0L) Traversable(ArbitBox(to.head._1, nonce, to.head._2))
    else Traversable[TokenBox]()

  override lazy val json: Json = Map( // tx in json form
    "txHash" -> id.toString.asJson,
    "txType" -> "Coinbase".asJson,
    "newBoxes" -> newBoxes.map(b => Base58.encode(b.id).asJson).toSeq.asJson,
    "to" -> to.map { s =>
      Map(
        "proposition" -> Base58.encode(to.head._1.pubKeyBytes).asJson,
        "value" -> to.head._2.asJson
      ).asJson
    }.asJson,
     "fee" -> fee.asJson,
    "signatures" -> signatures
      .map(s => Base58.encode(s.signature).asJson)
      .asJson,
    "timestamp" -> timestamp.asJson
  ).asJson


  // just tac on the byte string "Coinbase" to the beginning of the common message
  override lazy val messageToSign: Array[Byte] =
      "Coinbase".getBytes() ++
      super.messageToSign ++
      blockId.hashBytes

  override def toString: String = s"Coinbase(${json.noSpaces})"

}

object Coinbase {

  type SR = StateReader[Box]

  def nonceFromDigest(digest: Array[Byte]): Nonce = Longs.fromByteArray(digest.take(Longs.BYTES)) // take in a byte array and return a nonce (long)

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
    val sig = Map(rewardAddr ->  Signature25519(Array.empty[Byte]))
    Coinbase(to, sig, timestamp, parentId)
  }

  def syntacticValidate( tx: Coinbase, withSigs: Boolean = true): Try[Unit] = Try {
    require(tx.to.head._2 >= 0L) // can't make negative Arbits. anti-Arbits?!?!
    require(tx.fee == 0)
    require(tx.timestamp >= 0)
    require(tx.signatures.forall({ signature => // should be only one sig
      signature.isValid(tx.to.head._1, tx.messageToSign) // because this is set to self the signer is also the reciever
    }), "Invalid signature")
  }

  def validatePrototype(tx: Coinbase): Try[Unit] = syntacticValidate(tx, withSigs = false)

  def semanticValidate( tx: Coinbase, state: SR): Try[Unit] = {
    // check that the transaction is correctly formed before checking state
    syntacticValidate(tx)
  }
}
