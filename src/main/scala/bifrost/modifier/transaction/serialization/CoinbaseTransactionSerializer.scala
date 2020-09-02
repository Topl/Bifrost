package bifrost.modifier.transaction.serialization

import bifrost.crypto.Signature25519
import bifrost.crypto.serialization.Signature25519Serializer
import bifrost.modifier.box.proposition.{PublicKey25519Proposition, PublicKey25519PropositionSerializer}
import bifrost.modifier.transaction.bifrostTransaction.CoinbaseTransaction
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}
import bifrost.utils.Extensions._
import com.google.common.primitives.{Bytes, Ints, Longs}
import scorex.crypto.signatures.Curve25519

import scala.util.Try

object CoinbaseTransactionSerializer extends BifrostSerializer[CoinbaseTransaction] {

  override def serialize(obj: CoinbaseTransaction, w: Writer): Unit = {
    /* to: IndexedSeq[(PublicKey25519Proposition, Long)] */
    w.putUInt(obj.to.length)
    obj.to.foreach { case (prop, value) =>
      PublicKey25519PropositionSerializer.serialize(prop, w)
      w.putULong(value)
    }

    /* signatures: IndexedSeq[Signature25519] */
    w.putUInt(obj.signatures.size)
    obj.signatures.foreach(sig => Signature25519Serializer.serialize(sig, w))

    /* timestamp: Long */
    w.putULong(obj.timestamp)

    /* blockID: Array[Byte] */
    w.putUInt(obj.blockID.length)
    w.putBytes(obj.blockID)
  }

  override def parse(r: Reader): CoinbaseTransaction = {
    val toLength: Int = r.getUInt().toIntExact
    val to: IndexedSeq[(PublicKey25519Proposition, Long)] = (0 until toLength).map { _ =>
      val prop = PublicKey25519PropositionSerializer.parse(r)
      val value = r.getULong()
      prop -> value
    }

    val signaturesLength: Int = r.getUInt().toIntExact
    val signatures: IndexedSeq[Signature25519] = (0 until signaturesLength).map(_ => Signature25519Serializer.parse(r))

    val timestamp: Long = r.getULong()

    val blockIDLength: Int = r.getUInt().toIntExact
    val blockID: Array[Byte] = r.getBytes(blockIDLength)

    CoinbaseTransaction(to, signatures, timestamp, blockID)
  }
}
