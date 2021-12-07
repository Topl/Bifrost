package co.topl.codecs.binary.legacy.modifier.transaction

import co.topl.attestation._
import co.topl.modifier.box.SimpleValue
import co.topl.modifier.transaction.ArbitTransfer
import co.topl.utils.Extensions.LongOps
import co.topl.utils.Int128
import co.topl.utils.StringDataTypes.Latin1Data
import co.topl.codecs.binary.legacy._
import co.topl.codecs.binary.legacy.attestation._
import co.topl.codecs.binary.legacy.modifier.box.TokenValueHolderSerializer
import co.topl.codecs.binary.scodecs.valuetypes.Constants.stringCharacterSet

import scala.collection.immutable.ListMap
import scala.language.existentials

object ArbitTransferSerializer extends BifrostSerializer[ArbitTransfer[_ <: Proposition]] {

  def serialize(obj: ArbitTransfer[_ <: Proposition], w: Writer): Unit = {
    /* Byte */ // this is used to signal the types of propositions in the transactions
    w.put(obj.getPropIdentifier.typePrefix)

    /* from: IndexedSeq[(Address, Nonce)] */
    w.putUInt(obj.from.length)
    obj.from.foreach { case (addr, nonce) =>
      AddressSerializer.serialize(addr, w)
      w.putLong(nonce)
    }

    /* to: IndexedSeq[(Address, Long)] */
    w.putUInt(obj.to.length)
    obj.to.foreach { case (addr, value) =>
      AddressSerializer.serialize(addr, w)
      TokenValueHolderSerializer.serialize(value, w)
    }

    /* signatures: Map[Proposition, Proof] */
    w.putUInt(obj.attestation.size)
    obj.attestation.foreach { case (prop, sig) =>
      PropositionSerializer.serialize(prop, w)
      ProofSerializer.serialize(sig, w)
    }

    /* fee: Int128 */
    w.putInt128(obj.fee)

    /* timestamp: Long */
    w.putULong(obj.timestamp)

    /* data: Option[String] */
    w.putOption(obj.data) { (writer, d) =>
      writer.putByteString(new String(d.value, stringCharacterSet))
    }

    /* minting: Boolean */
    w.putBoolean(obj.minting)
  }

  override def parse(r: Reader): ArbitTransfer[_ <: Proposition] = {
    val propTypePrefix = r.getByte()

    val fromLength: Int = r.getUInt().toIntExact
    val from = (0 until fromLength).map { _ =>
      val addr = AddressSerializer.parse(r)
      val nonce = r.getLong()
      addr -> nonce
    }

    val toLength: Int = r.getUInt().toIntExact
    val to = (0 until toLength).map { _ =>
      val addr = AddressSerializer.parse(r)
      val value = TokenValueHolderSerializer.parse(r) match {
        case v: SimpleValue => v
        case _              => throw new Exception("Invalid TokenValueHolder for ArbitTransfer")
      }
      addr -> value
    }

    val signaturesLength: Int = r.getUInt().toIntExact
    val signatures = ListMap((0 until signaturesLength).map { _ =>
      val prop = PropositionSerializer.parse(r)
      val sig = ProofSerializer.parse(r)
      prop -> sig
    }: _*)

    val fee: Int128 = r.getInt128()
    val timestamp: Long = r.getULong()

    val data: Option[Latin1Data] = r.getOption {
      Latin1Data.unsafe(r.getByteString())
    }

    val minting: Boolean = r.getBoolean()

    propTypePrefix match {
      case PublicKeyPropositionCurve25519.`typePrefix` =>
        val sigs = signatures.asInstanceOf[ListMap[PublicKeyPropositionCurve25519, SignatureCurve25519]]
        ArbitTransfer(from, to, sigs, fee, timestamp, data, minting)

      case ThresholdPropositionCurve25519.`typePrefix` =>
        val sigs = signatures.asInstanceOf[ListMap[ThresholdPropositionCurve25519, ThresholdSignatureCurve25519]]
        ArbitTransfer(from, to, sigs, fee, timestamp, data, minting)

      case PublicKeyPropositionEd25519.`typePrefix` =>
        val sigs = signatures.asInstanceOf[ListMap[PublicKeyPropositionEd25519, SignatureEd25519]]
        ArbitTransfer(from, to, sigs, fee, timestamp, data, minting)
    }
  }
}
