package co.topl.modifier.transaction.serialization

import co.topl.attestation._
import co.topl.attestation.serialization.{ProofSerializer, PropositionSerializer}
import co.topl.modifier.transaction.ArbitTransfer
import co.topl.nodeView.state.box.TokenValueHolder
import co.topl.utils.Extensions._
import co.topl.utils.serialization.{BifrostSerializer, Reader, Writer}

import scala.language.existentials

object ArbitTransferSerializer extends BifrostSerializer[ArbitTransfer[_ <: Proposition]] {

  def serialize(obj: ArbitTransfer[_ <: Proposition], w: Writer): Unit = {
    /* Byte */ //this is used to signal the types of propositions in the transactions
    w.put(obj.attestation.head._1.propTypePrefix)

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
      TokenValueHolder.serialize(value, w)
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

  override def parse(r: Reader): ArbitTransfer[_ <: Proposition] = {
    val propTypePrefix = r.getByte()

    val fromLength: Int = r.getUInt().toIntExact
    val from = (0 until fromLength).map { _ =>
      val addr = Address.parse(r)
      val nonce = r.getLong()
      addr -> nonce
    }

    val toLength: Int = r.getUInt().toIntExact
    val to = (0 until toLength).map { _ =>
      val addr = Address.parse(r)
      val value = TokenValueHolder.parse(r)
      addr -> value
    }

    val signaturesLength: Int = r.getUInt().toIntExact
    val signatures = Map((0 until signaturesLength).map { _ =>
      val prop = PropositionSerializer.parse(r)
      val sig = ProofSerializer.parse(r)
      prop -> sig
    }: _*)

    val fee: Long = r.getULong()
    val timestamp: Long = r.getULong()
    val data: String = r.getIntString()
    val minting: Boolean = r.getBoolean()

    propTypePrefix match {
      case PublicKeyPropositionCurve25519.typePrefix =>
        require(signatures.forall(_._1.propTypePrefix == PublicKeyPropositionCurve25519.typePrefix))
        val sigs = signatures.asInstanceOf[Map[PublicKeyPropositionCurve25519, SignatureCurve25519]]
        ArbitTransfer(from, to, sigs, fee, timestamp, data, minting)

      case ThresholdPropositionCurve25519.typePrefix =>
        require(signatures.forall(_._1.propTypePrefix == ThresholdPropositionCurve25519.typePrefix))
        val sigs = signatures.asInstanceOf[Map[ThresholdPropositionCurve25519, ThresholdSignatureCurve25519]]
        ArbitTransfer(from, to, sigs, fee, timestamp, data, minting)
    }
  }
}
