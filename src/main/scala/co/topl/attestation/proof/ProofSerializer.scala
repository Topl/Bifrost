package co.topl.attestation.proof

import co.topl.attestation.proposition.{Proposition, PublicKeyCurve25519Proposition, ThresholdCurve25519Proposition}
import co.topl.utils.serialization.{BifrostSerializer, Reader, Writer}

object ProofSerializer extends BifrostSerializer[Proof[_ <: Proposition]] {
  def serialize(obj: Proof[_ <: Proposition], w: Writer): Unit = {
    obj match {
      case obj: SignatureCurve25519 =>
        w.put(PublicKeyCurve25519Proposition.typePrefix)
        SignatureCurve25519Serializer.serialize(obj, w)

      case obj: ThresholdSignatureCurve25519 =>
        w.put(ThresholdCurve25519Proposition.typePrefix)
        ThresholdSignatureCurve25519Serializer.serialize(obj, w)
    }
  }

  def parse(r: Reader): Proof[_ <: Proposition] = {
    r.getByte() match {
      case PublicKeyCurve25519Proposition.typePrefix => SignatureCurve25519Serializer.parse(r)
      case ThresholdCurve25519Proposition.typePrefix => ThresholdSignatureCurve25519Serializer.parse(r)
    }
  }
}
