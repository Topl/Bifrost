package co.topl.modifier.transaction.serialization

import co.topl.attestation.proposition.{PublicKeyPropositionCurve25519, PublicKeyPropositionCurve25519Serializer}
import co.topl.attestation.proof.{SignatureCurve25519, SignatureCurve25519Serializer}
import co.topl.modifier.transaction.CodeCreation
import co.topl.utils.serialization.{BifrostSerializer, Reader, Writer}

object CodeBoxCreationSerializer extends BifrostSerializer[CodeCreation]{

  override def serialize(obj: CodeCreation, w: Writer): Unit = {
    w.putByteString("CodeCreation")

    /* to: PublicKey25519Proposition */
    PublicKeyPropositionCurve25519Serializer.serialize(obj.to, w)

    /* signature: Signature25519 */
    SignatureCurve25519Serializer.serialize(obj.signature, w)

    /* code: String */
    w.putIntString(obj.code)

    /* override val fee: Long */
    w.putULong(obj.fee)

    /* override val timestamp: Long */
    w.putULong(obj.timestamp)

    /* data: String */
    w.putIntString(obj.data)
  }

  override def parse(r: Reader): CodeCreation = {
    require(r.getByteString() == "CodeCreation")

    val to: PublicKeyPropositionCurve25519 = PublicKeyPropositionCurve25519Serializer.parse(r)
    val signature: SignatureCurve25519 = SignatureCurve25519Serializer.parse(r)
    val code: String = r.getIntString()
    val fee: Long = r.getULong()
    val timestamp: Long = r.getULong()
    val data: String = r.getIntString()

    CodeCreation(to, signature, code, fee, timestamp, data)
  }
}
