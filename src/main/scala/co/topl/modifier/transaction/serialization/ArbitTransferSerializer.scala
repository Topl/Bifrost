package co.topl.modifier.transaction.serialization

import co.topl.attestation.Address
import co.topl.attestation.proof.{Proof, ProofSerializer}
import co.topl.attestation.proposition.{Proposition, PropositionSerializer}
import co.topl.modifier.transaction.ArbitTransfer
import co.topl.utils.Extensions._
import co.topl.utils.serialization.{BifrostSerializer, Reader, Writer}

object ArbitTransferSerializer extends BifrostSerializer[ArbitTransfer[_ <: Proposition, _ <: Proof[_]]] {

  override def serialize(obj: ArbitTransfer[_ <: Proposition, _ <: Proof[_]], w: Writer): Unit = {
    /* from: IndexedSeq[(PublicKey25519Proposition, Nonce)] */
    w.putUInt(obj.from.length)
    obj.from.foreach { case (addr, nonce) =>
      Address.serialize(addr, w)
      w.putLong(nonce)
    }

    /* to: IndexedSeq[(PublicKey25519Proposition, Long)] */
    w.putUInt(obj.to.length)
    obj.to.foreach { case (addr, value) =>
      Address.serialize(addr, w)
      w.putLong(value)
    }

    /* signatures: Map[PublicKey25519Proposition, Signature25519] */
    w.putUInt(obj.signatures.size)
    obj.signatures.foreach { case (prop, sig) =>
      PropositionSerializer.serialize(prop, w)
      ProofSerializer.serialize(sig, w)
    }

    /* fee: Long */
    w.putULong(obj.fee)

    /* timestamp: Long */
    w.putULong(obj.timestamp)

    /* data: String */
    w.putIntString(obj.data)
  }

  override def parse(r: Reader): ArbitTransfer[_ <: Proposition, _ <: Proof[_]] = {
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

    new ArbitTransfer(signatures, from, to, fee, timestamp, data)
  }
}
