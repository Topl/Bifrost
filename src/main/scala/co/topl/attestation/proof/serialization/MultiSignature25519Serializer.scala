package co.topl.attestation.proof.serialization

import co.topl.attestation.proof
import co.topl.attestation.proof.{ MultiSignature25519, Signature25519 }
import co.topl.utils.Extensions._
import co.topl.utils.serialization.{ BifrostSerializer, Reader, Writer }


object MultiSignature25519Serializer extends BifrostSerializer[MultiSignature25519] {

  override def serialize(obj: MultiSignature25519, w: Writer): Unit = {
    /* signatureSet: Set[Signature25519] */
    w.putUInt(obj.signatureSet.size)
    obj.signatureSet.foreach(sig => Signature25519Serializer.serialize(sig, w))
  }

  override def parse(r: Reader): MultiSignature25519 = {
    val signatureSetLength: Int = r.getUInt().toIntExact
    val signatureSet: Set[Signature25519] = (0 until signatureSetLength).map(_ => Signature25519Serializer.parse(r)).toSet

    proof.MultiSignature25519(signatureSet)
  }
}
