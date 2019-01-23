package bifrost.transaction.bifrostTransaction

import java.time.Instant

import bifrost.crypto.hash.FastCryptographicHash
import BifrostTransaction.{Nonce, Value}
import bifrost.transaction.box.proposition.PublicKey25519Proposition
import bifrost.transaction.box.{BifrostBox, PolyBox}
import bifrost.transaction.proof.Signature25519
import bifrost.transaction.serialization.PolyTransferCompanion
import bifrost.transaction.state.PrivateKey25519
import bifrost.wallet.BWallet
import com.google.common.primitives.Ints

import scala.util.Try

case class PolyTransfer(override val from: IndexedSeq[(PublicKey25519Proposition, Nonce)],
                        override val to: IndexedSeq[(PublicKey25519Proposition, Long)],
                        override val signatures: IndexedSeq[Signature25519],
                        override val fee: Long,
                        override val timestamp: Long,
                        val data: String)
  extends TransferTransaction(from, to, signatures, fee, timestamp) {

  override type M = PolyTransfer

  override lazy val serializer = PolyTransferCompanion

  override def toString: String = s"PolyTransfer(${json.noSpaces})"

  override lazy val newBoxes: Traversable[BifrostBox] = to.zipWithIndex.map {
    case ((prop, value), idx) =>
      val nonce = PolyTransfer
        .nonceFromDigest(FastCryptographicHash("PolyTransfer".getBytes
          ++ prop.pubKeyBytes
          ++ hashNoNonces
          ++ Ints.toByteArray(idx)))

      PolyBox(prop, nonce, value)
  }

  override lazy val messageToSign: Array[Byte] = "PolyTransfer".getBytes() ++ super.commonMessageToSign ++ data.getBytes
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

  def create(w: BWallet, toReceive: IndexedSeq[(PublicKey25519Proposition, Long)], fee: Long, data: String, publicKeyToSendFrom: Vector[String] = Vector(), publicKeyToSendChangeTo: String = "") = Try {
    val params = parametersForCreate(w, toReceive, fee, "PolyTransfer", publicKeyToSendFrom, publicKeyToSendChangeTo)
    val timestamp = Instant.now.toEpochMilli
    PolyTransfer(params._1.map(t => t._1 -> t._2), params._2, fee, timestamp, data)
  }

//  def createByKey(w: BWallet, toReceive: IndexedSeq[(PublicKey25519Proposition, Long)], fee: Long, data: String, publicKeyToSendFrom: Seq[Json]) = Try {
//        println()
//        println("Entered createByKey")
//        val params = parametersForCreate(w, toReceive, fee, "PolyTransfer", "")
//        val timestamp = Instant.now.toEpochMilli
//        PolyTransfer(params._1.map(t => t._1 -> t._2), params._2, fee, timestamp, data)
//      }//

  def validate(tx: PolyTransfer): Try[Unit] = validateTx(tx)
}