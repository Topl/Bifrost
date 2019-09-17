package bifrost.transaction.bifrostTransaction

import java.time.Instant

import bifrost.crypto.hash.FastCryptographicHash
import BifrostTransaction.{Nonce, Value}
import bifrost.tokenBoxRegistry.TokenBoxRegistry
import bifrost.transaction.box.proposition.PublicKey25519Proposition
import bifrost.transaction.box.{BifrostBox, PolyBox}
import bifrost.transaction.proof.Signature25519
import bifrost.transaction.serialization.PolyTransferCompanion
import bifrost.transaction.state.PrivateKey25519
import bifrost.wallet.BWallet
import com.google.common.primitives.Ints
import io.circe.Json

import scala.util.Try

case class PolyTransfer(override val from: IndexedSeq[(PublicKey25519Proposition, Nonce)],
                        override val to: IndexedSeq[(PublicKey25519Proposition, Long)],
                        override val signatures: Map[PublicKey25519Proposition, Signature25519],
                        override val fee: Long,
                        override val timestamp: Long,
                        override val data: String)
  extends TransferTransaction(from, to, signatures, fee, timestamp, data) {

  override type M = PolyTransfer

  override lazy val serializer = PolyTransferCompanion

  override def toString: String = s"PolyTransfer(${json.noSpaces})"

  override lazy val newBoxes: Traversable[BifrostBox] = to
    .filter(toInstance => toInstance._2 > 0L)
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

  override lazy val messageToSign: Array[Byte] = "PolyTransfer".getBytes() ++ super.commonMessageToSign

  override lazy val json: Json = super.json("PolyTransfer")

}

object PolyTransfer extends TransferUtil {

  def apply(from: IndexedSeq[(PrivateKey25519, Nonce)],
            to: IndexedSeq[(PublicKey25519Proposition, Value)],
            fee: Long,
            timestamp: Long,
            data: String): PolyTransfer = {
    val params = parametersForApply(from, to, fee, timestamp, "PolyTransfer", data).get
    PolyTransfer(params._1, to, params._2, fee, timestamp, data)
  }

  def create(tbr: TokenBoxRegistry,
             w: BWallet,
             toReceive: IndexedSeq[(PublicKey25519Proposition, Long)],
             sender: IndexedSeq[PublicKey25519Proposition],
             fee: Long, data: String): Try[PolyTransfer] = Try {
    val params = parametersForCreate(tbr, w, toReceive, sender, fee, "PolyTransfer")
    val timestamp = Instant.now.toEpochMilli
    PolyTransfer(params._1.map(t => t._1 -> t._2), params._2, fee, timestamp, data)
  }

  def createPrototype(tbr: TokenBoxRegistry,
                      toReceive: IndexedSeq[(PublicKey25519Proposition, Long)],
                      sender: IndexedSeq[PublicKey25519Proposition],
                      fee: Long,
                      data: String): Try[PolyTransfer] = Try
  {
    val params = parametersForCreate(tbr, toReceive, sender, fee, "PolyTransfer")
    val timestamp = Instant.now.toEpochMilli
    PolyTransfer(params._1.map(t => t._1 -> t._2), params._2, Map(), fee, timestamp, data)
  }

  def validate(tx: PolyTransfer): Try[Unit] = validateTx(tx)

  def validatePrototype(tx: PolyTransfer): Try[Unit] = validateTxWithoutSignatures(tx)

}