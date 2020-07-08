package bifrost.modifier.transaction.bifrostTransaction

import java.time.Instant

import bifrost.crypto.{FastCryptographicHash, PrivateKey25519, Signature25519}
import bifrost.modifier.box.proposition.PublicKey25519Proposition
import bifrost.modifier.box.{ArbitBox, Box}
import bifrost.modifier.transaction.bifrostTransaction.Transaction.{Nonce, Value}
import bifrost.modifier.transaction.serialization.ArbitTransferCompanion
import bifrost.state.TokenBoxRegistry
import bifrost.wallet.Wallet
import com.google.common.primitives.Ints
import io.circe.Json

import scala.util.Try

case class ArbitTransfer(override val from: IndexedSeq[(PublicKey25519Proposition, Nonce)],
                         override val to: IndexedSeq[(PublicKey25519Proposition, Long)],
                         override val signatures: Map[PublicKey25519Proposition, Signature25519],
                         override val fee: Long,
                         override val timestamp: Long,
                         override val data: String)
  extends TransferTransaction(from, to, signatures, fee, timestamp, data) {

  override type M = ArbitTransfer

  override lazy val serializer = ArbitTransferCompanion

  override def toString: String = s"ArbitTransfer(${json.noSpaces})"

  override lazy val newBoxes: Traversable[Box] = to
    .filter(toInstance => toInstance._2 > 0L)
    .zipWithIndex
    .map {
    case ((prop, value), idx) =>
      val nonce = ArbitTransfer
        .nonceFromDigest(FastCryptographicHash("ArbitTransfer".getBytes
          ++ prop.pubKeyBytes
          ++ hashNoNonces
          ++ Ints.toByteArray(idx)))

      ArbitBox(prop, nonce, value)
  }

  override lazy val messageToSign: Array[Byte] = "ArbitTransfer".getBytes ++ super.commonMessageToSign

  override lazy val json: Json = super.json("ArbitTransfer")
}

//noinspection ScalaStyle
object ArbitTransfer extends TransferUtil {

  def apply(from: IndexedSeq[(PrivateKey25519, Nonce)],
            to: IndexedSeq[(PublicKey25519Proposition, Value)],
            fee: Long,
            timestamp: Long,
            data: String): ArbitTransfer = {
    val params = parametersForApply(from, to, fee, timestamp, "ArbitTransfer", data).get
    ArbitTransfer(params._1, to, params._2, fee, timestamp, data)
  }

  def create(tbr: TokenBoxRegistry, w: Wallet, toRecieve: IndexedSeq[(PublicKey25519Proposition, Long)], sender: IndexedSeq[PublicKey25519Proposition], fee: Long, data: String): Try[ArbitTransfer] = Try
  {

    val params = parametersForCreate(tbr, w, toRecieve, sender, fee, "ArbitTransfer")
    val timestamp = Instant.now.toEpochMilli
    ArbitTransfer(params._1.map(t => t._1 -> t._2), params._2, fee, timestamp, data)
  }

  def createPrototype(tbr: TokenBoxRegistry, toReceive: IndexedSeq[(PublicKey25519Proposition, Long)], sender: IndexedSeq[PublicKey25519Proposition], fee: Long, data: String): Try[ArbitTransfer] = Try
  {
    val params = parametersForCreate(tbr, toReceive, sender, fee, "ArbitTransfer")
    val timestamp = Instant.now.toEpochMilli
    ArbitTransfer(params._1.map(t => t._1 -> t._2), params._2, Map(), fee, timestamp, data)
  }

  def validate(tx: ArbitTransfer): Try[Unit] = validateTx(tx)

  def validatePrototype(tx: ArbitTransfer): Try[Unit] = validateTxWithoutSignatures(tx)

}