package co.topl.modifier.transaction.serialization

import co.topl.attestation.proposition.{PublicKeyCurve25519Proposition, PublicKeyCurve25519PropositionSerializer}
import co.topl.attestation.proof.{SignatureCurve25519, SignatureCurve25519Serializer}
import co.topl.modifier.transaction.AssetCreation
import co.topl.utils.Extensions._
import co.topl.utils.serialization.{BifrostSerializer, Reader, Writer}

object AssetCreationSerializer extends BifrostSerializer[AssetCreation] {

  override def serialize(obj: AssetCreation, w: Writer): Unit = {
    /* to: IndexedSeq[(PublicKey25519Proposition, Long)] */
    w.putUInt(obj.to.length)
    obj.to.foreach { case (prop, value) =>
      PublicKeyCurve25519PropositionSerializer.serialize(prop, w)
      w.putULong(value)
    }

    /* signatures: Map[PublicKey25519Proposition, Signature25519] */
    w.putUInt(obj.signatures.size)
    obj.signatures.foreach { case (prop, sig) =>
      PublicKeyCurve25519PropositionSerializer.serialize(prop, w)
      SignatureCurve25519Serializer.serialize(sig, w)
    }

    /* assetCode: String */
    w.putIntString(obj.assetCode)

    /* issuer: PublicKey25519Proposition */
    PublicKeyCurve25519PropositionSerializer.serialize(obj.issuer, w)

    /* fee: Long */
    w.putULong(obj.fee)

    /* timestamp: Long */
    w.putULong(obj.timestamp)

    /* data: String */
    w.putIntString(obj.data)
  }

  override def parse(r: Reader): AssetCreation = {
    val toLength: Int = r.getUInt().toIntExact
    val to: IndexedSeq[(PublicKeyCurve25519Proposition, Long)] = (0 until toLength).map { _ =>
      val prop = PublicKeyCurve25519PropositionSerializer.parse(r)
      val value = r.getULong()
      prop -> value
    }

    val signaturesLength: Int = r.getUInt().toIntExact
    val signatures: Map[PublicKeyCurve25519Proposition, SignatureCurve25519] = (0 until signaturesLength).map { _ =>
      val prop = PublicKeyCurve25519PropositionSerializer.parse(r)
      val sig = SignatureCurve25519Serializer.parse(r)
      prop -> sig
    }.toMap

    val assetCode: String = r.getIntString()
    val issuer: PublicKeyCurve25519Proposition = PublicKeyCurve25519PropositionSerializer.parse(r)
    val fee: Long = r.getULong()
    val timestamp: Long = r.getULong()
    val data: String = r.getIntString()

    AssetCreation(to, signatures, assetCode, issuer, fee, timestamp, data)
  }
}
