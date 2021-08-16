package co.topl.attestation.serialization

import co.topl.attestation._
import co.topl.utils.serialization.{BifrostSerializer, Reader, Writer}

object ProofSerializer extends BifrostSerializer[Proof[_]] {

  def serialize(obj: Proof[_], w: Writer): Unit =
    obj match {
      case obj: SignatureCurve25519 =>
        w.put(PublicKeyPropositionCurve25519.typePrefix)
        SignatureCurve25519Serializer.serialize(obj, w)

      case obj: ThresholdSignatureCurve25519 =>
        w.put(ThresholdPropositionCurve25519.typePrefix)
        ThresholdSignatureCurve25519Serializer.serialize(obj, w)

      case obj: SignatureEd25519 =>
        w.put(PublicKeyPropositionEd25519.typePrefix)
        SignatureEd25519Serializer.serialize(obj, w)
    }

  def parse(r: Reader): Proof[_ <: Proposition] =
    r.getByte() match {
      case PublicKeyPropositionCurve25519.typePrefix => SignatureCurve25519Serializer.parse(r)
      case ThresholdPropositionCurve25519.typePrefix => ThresholdSignatureCurve25519Serializer.parse(r)
      case PublicKeyPropositionEd25519.typePrefix    => SignatureEd25519Serializer.parse(r)
    }
}
