package co.topl.modifier.transaction.serialization

import co.topl.crypto.Signature25519
import co.topl.crypto.serialization.Signature25519Serializer
import co.topl.modifier.transaction.CoinbaseTransaction
import co.topl.nodeView.state.box.proposition.{ PublicKey25519Proposition, PublicKey25519PropositionSerializer }
import co.topl.utils.Extensions._
import co.topl.utils.serialization.{ BifrostSerializer, Reader, Writer }

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
