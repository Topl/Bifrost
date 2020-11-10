package co.topl.attestation.proof

import co.topl.attestation.proposition.{Proposition, PublicKeyPropositionCurve25519, ThresholdPropositionCurve25519}
import co.topl.utils.serialization.{BifrostSerializer, Reader, Writer}

object ProofSerializer extends BifrostSerializer[Proof[_ <: Proposition]] {
  def serialize(obj: Proof[_ <: Proposition], w: Writer): Unit = {
    obj match {
      case obj: SignatureCurve25519 =>
        w.put(PublicKeyPropositionCurve25519.typePrefix)
        SignatureCurve25519Serializer.serialize(obj, w)

      case obj: ThresholdSignatureCurve25519 =>
        w.put(ThresholdPropositionCurve25519.typePrefix)
        ThresholdSignatureCurve25519Serializer.serialize(obj, w)
    }
  }

  def parse(r: Reader): Proof[_ <: Proposition] = {
    r.getByte() match {
      case PublicKeyPropositionCurve25519.typePrefix => SignatureCurve25519Serializer.parse(r)
      case ThresholdPropositionCurve25519.typePrefix => ThresholdSignatureCurve25519Serializer.parse(r)
    }
  }
}
