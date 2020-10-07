package bifrost.modifier.transaction.bifrostTransaction

import java.time.Instant

import bifrost.crypto.{ FastCryptographicHash, PrivateKey25519, Signature25519 }
import bifrost.modifier.box.proposition.PublicKey25519Proposition
import bifrost.modifier.box.{ ArbitBox, Box, TokenBox }
import bifrost.modifier.transaction.bifrostTransaction.Transaction.Nonce
import bifrost.modifier.transaction.serialization.CoinbaseTransactionSerializer
import bifrost.state.StateReader
import bifrost.utils.serialization.BifrostSerializer
import bifrost.wallet.Wallet
import com.google.common.primitives.{ Bytes, Longs }
import io.circe.Json
import io.circe.syntax._
import scorex.util.encode.Base58
import scorex.crypto.hash.Digest32
import scorex.crypto.signatures.Signature

import scala.util.Try

case class CoinbaseTransaction (to: IndexedSeq[(PublicKey25519Proposition, Long)],
                                signatures: IndexedSeq[Signature25519],
                                override val timestamp: Long,
                                blockID: Array[Byte]) extends Transaction {
  override type M = CoinbaseTransaction

  lazy val serializer: BifrostSerializer[CoinbaseTransaction] = CoinbaseTransactionSerializer

  lazy val fee = 0L // you don't ever pay for a Coinbase TX since you'd be paying yourself so fee must equal 0

  override lazy val boxIdsToOpen: IndexedSeq[Array[Byte]] = IndexedSeq()

  lazy val hashNoNonces: Digest32 = FastCryptographicHash(
    to.head._1.pubKeyBytes ++ Longs.toByteArray(timestamp) ++ Longs.toByteArray(fee) ++ blockID // message that gets hashed
  )

  val nonce: Nonce = CoinbaseTransaction.nonceFromDigest(FastCryptographicHash(
    "CoinbaseTransaction".getBytes ++ hashNoNonces
  ))

  lazy val newBoxes: Traversable[TokenBox] =
    if (to.head._2 > 0L) Traversable(ArbitBox(to.head._1, nonce, to.head._2))
    else Traversable[TokenBox]()

  override lazy val json: Json = Map( // tx in json form
    "txHash" -> id.toString.asJson,
    "txType" -> "CoinbaseTransaction".asJson,
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


  override lazy val messageToSign: Array[Byte] = Bytes.concat( // just tac on the byte string "CoinbaseTransaction" to the beginning of the common message
    "CoinbaseTransaction".getBytes(),
    newBoxes.foldLeft(Array[Byte]())((acc, x) => acc ++ x.bytes),
    Longs.toByteArray(fee),
    blockID
  )

  override def toString: String = s"CoinbaseTransaction(${json.noSpaces})"

  def commonMessageToSign: Array[Byte] =
    if(newBoxes.nonEmpty) {
      newBoxes.head.bytes}
    else {
      Array[Byte]()} ++ // is the new box + the timestamp + the fee,
      Longs.toByteArray(timestamp) ++
      Longs.toByteArray(fee) ++
      blockID
}

object CoinbaseTransaction {

  type SR = StateReader[Box]

  def nonceFromDigest(digest: Array[Byte]): Nonce = Longs.fromByteArray(digest.take(Longs.BYTES)) // take in a byte array and return a nonce (long)

  def createAndApply(w: Wallet,
                     to: IndexedSeq[(PublicKey25519Proposition, Long)],
                     blockID: Array[Byte] // the blockID of the parent block. EX: if this is the CB for block 100 the blockID would be the id of block 99
                    ): Try[CoinbaseTransaction] = Try {
    val selectedSecret = w.secretByPublicImage(to.head._1).get // use the receiver's pub-key to generate secret
    val fakeSigs = IndexedSeq(Signature25519(Signature @@ Array.emptyByteArray)) // create an index sequence of empty sigs
    val timestamp = Instant.now.toEpochMilli // generate timestamp
    val messageToSign = CoinbaseTransaction(to, fakeSigs, timestamp, blockID).messageToSign // using your fake sigs generate a CB tx and get its msg to sign
    val signatures = IndexedSeq(PrivateKey25519.sign(selectedSecret, messageToSign)) // sign the msg you just generated
    CoinbaseTransaction(to, signatures, timestamp, blockID) // use the sigs you just generated to make the real CB tx
  }

  def syntacticValidate(tx: CoinbaseTransaction, withSigs: Boolean = true): Try[Unit] = Try {
    require(tx.to.head._2 >= 0L) // can't make negative Arbits. anti-Arbits?!?!
    require(tx.fee == 0)
    require(tx.timestamp >= 0)
    require(tx.signatures.forall({ signature => // should be only one sig
      signature.isValid(tx.to.head._1, tx.messageToSign) // because this is set to self the signer is also the reciever
    }), "Invalid signature")
  }

  def validatePrototype(tx: CoinbaseTransaction): Try[Unit] = syntacticValidate(tx, withSigs = false)

  def semanticValidate(tx: CoinbaseTransaction, state: SR): Try[Unit] = {
    // check that the transaction is correctly formed before checking state
    syntacticValidate(tx)
  }
}
