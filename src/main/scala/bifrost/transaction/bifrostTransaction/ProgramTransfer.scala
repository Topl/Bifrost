package bifrost.transaction.bifrostTransaction

import java.time.Instant
import java.util.UUID

import bifrost.crypto.hash.FastCryptographicHash
import bifrost.serialization.Serializer
import bifrost.transaction.bifrostTransaction.BifrostTransaction.Nonce
import bifrost.transaction.box.{BifrostBox, ExecutionBox}
import bifrost.transaction.box.proposition.PublicKey25519Proposition
import bifrost.transaction.proof.Signature25519
import bifrost.transaction.serialization.ProgramTransferCompanion
import bifrost.transaction.state.PrivateKey25519Companion
import bifrost.wallet.BWallet
import com.google.common.primitives.{Bytes, Longs}
import io.circe.Json
import io.circe.syntax._
import scorex.crypto.encode.Base58

import scala.util.Try

case class ProgramTransfer(from: PublicKey25519Proposition,
                           to: PublicKey25519Proposition,
                           signature: Signature25519,
                           executionBox: ExecutionBox,
                           fee: Long,
                           timestamp: Long,
                           data: String) extends BifrostTransaction {

  override type M = ProgramTransfer

  override lazy val serializer: Serializer[ProgramTransfer] = ProgramTransferCompanion

  override def toString: String = s"ProgramTransfer(${json.noSpaces})"

  lazy val hashNoNonces: Array[Byte] = FastCryptographicHash(
    to.pubKeyBytes
      ++ Longs.toByteArray(fee)
      ++ data.getBytes
  )

  override lazy val boxIdsToOpen: IndexedSeq[Array[Byte]] = IndexedSeq(executionBox.id)

  override lazy val newBoxes: Traversable[BifrostBox] = {

    val nonce = ProgramTransfer.nonceFromDigest(
      FastCryptographicHash("ProgramTransfer".getBytes
        ++ to.pubKeyBytes
        ++ hashNoNonces
      ))

    val uuid = UUID.nameUUIDFromBytes(ExecutionBox.idFromBox(to, nonce))

    Seq(ExecutionBox(to, nonce, uuid, executionBox.stateBoxUUIDs, executionBox.codeBoxIds))
  }

  override lazy val messageToSign: Array[Byte] = Bytes.concat(
    "ProgramTransfer".getBytes
      ++ to.pubKeyBytes
      ++ newBoxes.head.bytes
      ++ Longs.toByteArray(fee)
  )

  override lazy val json: Json = Map(
    "txHash" -> Base58.encode(id).asJson,
    "txType" -> "ProgramTransfer".asJson,
    "newBoxes" -> Base58.encode(newBoxes.head.id).asJson,
    "boxesToRemove" -> Base58.encode(boxIdsToOpen.head).asJson,
    "from" -> Base58.encode(from.pubKeyBytes).asJson,
    "to" -> Base58.encode(to.pubKeyBytes).asJson,
    "signature" -> Base58.encode(signature.signature).asJson,
    "fee" -> fee.asJson,
    "timestamp" -> timestamp.asJson,
    "data" -> data.asJson
  ).asJson
}

object ProgramTransfer {

  def nonceFromDigest(digest: Array[Byte]): Nonce = Longs.fromByteArray(digest.take(8))

  def createAndApply(w: BWallet,
                     from: PublicKey25519Proposition,
                     to: PublicKey25519Proposition,
                     executionBox: ExecutionBox,
                     fee: Long,
                     data: String): Try[ProgramTransfer] = Try {

    val selectedSecret = w.secretByPublicImage(from).get
    val fakeSig = Signature25519(Array())
    val timestamp = Instant.now.toEpochMilli
    val messageToSign = ProgramTransfer(from, to, fakeSig, executionBox, fee, timestamp, data).messageToSign

    val signature = PrivateKey25519Companion.sign(selectedSecret, messageToSign)

    ProgramTransfer(from, to, signature, executionBox, fee, timestamp, data)
  }

  //TODO implement prototype tx
  /*def createPrototype(tokenBoxRegistry: TokenBoxRegistry,
                      toReceive: IndexedSeq[(PublicKey25519Proposition, Long)],
                      sender: IndexedSeq[PublicKey25519Proposition],
                      fee: Long, data: String): Try[PolyTransfer] = Try
  {
    val params = parametersForCreate(tokenBoxRegistry, toReceive, sender, fee, "ProgramTransfer")
    val timestamp = Instant.now.toEpochMilli
    ProgramTransfer(params._1.map(t => t._1 -> t._2), params._2, Map(), fee, timestamp, data)
  }
   */

  def validate(tx: ProgramTransfer): Try[Unit] = Try {
    require(tx.fee >= 0)
    require(tx.timestamp >= 0)
    require(tx.signature.isValid(tx.from, tx.messageToSign))
  }
}
