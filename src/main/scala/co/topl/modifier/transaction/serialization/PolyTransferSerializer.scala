package co.topl.modifier.transaction.serialization

import co.topl.attestation.Address
import co.topl.attestation.proof.ProofSerializer
import co.topl.attestation.proposition.{ Proposition, PropositionSerializer }
import co.topl.modifier.transaction.PolyTransfer
import co.topl.utils.Extensions._
import co.topl.utils.serialization.{ BifrostSerializer, Reader, Writer }

object PolyTransferSerializer extends BifrostSerializer[PolyTransfer[_ <: Proposition]] {

  override def serialize(obj: PolyTransfer[_ <: Proposition], w: Writer): Unit = {
    /* from: IndexedSeq[(Address, Nonce)] */
    w.putUInt(obj.from.length)
    obj.from.foreach { case (addr, nonce) =>
      Address.serialize(addr, w)
      w.putLong(nonce)
    }

    /* to: IndexedSeq[(Address, Long)] */
    w.putUInt(obj.to.length)
    obj.to.foreach { case (addr, value) =>
      Address.serialize(addr, w)
      w.putLong(value)
    }

    /* signatures: Map[Proposition, Proof] */
    w.putUInt(obj.attestation.size)
    obj.attestation.foreach { case (prop, sig) =>
      PropositionSerializer.serialize(prop, w)
      ProofSerializer.serialize(sig, w)
    }

    /* fee: Long */
    w.putULong(obj.fee)

    /* timestamp: Long */
    w.putULong(obj.timestamp)

    /* data: String */
    w.putIntString(obj.data)

    /* minting: Boolean */
    w.putBoolean(obj.minting)
  }

  override def parse(r: Reader): PolyTransfer[_ <: Proposition] = {
    val fromLength: Int = r.getUInt().toIntExact
    val from = (0 until fromLength).map { _ =>
      val addr = Address.parse(r)
      val nonce = r.getLong()
      addr -> nonce
    }

    val toLength: Int = r.getUInt().toIntExact
    val to = (0 until toLength).map { _ =>
      val addr = Address.parse(r)
      val value = r.getULong()
      addr -> value
    }

    val signaturesLength: Int = r.getUInt().toIntExact
    val signatures = (0 until signaturesLength).map { _ =>
      val prop = PropositionSerializer.parse(r)
      val sig = ProofSerializer.parse(r)
      prop -> sig
    }.toMap

    val fee: Long = r.getULong()
    val timestamp: Long = r.getULong()
    val data: String = r.getIntString()
    val minting: Boolean = r.getBits(1).head

    PolyTransfer(from, to, signatures, fee, timestamp, data, minting)
  }
}
