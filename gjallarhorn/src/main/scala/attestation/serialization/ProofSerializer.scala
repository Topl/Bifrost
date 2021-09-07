package attestation.serialization

/**
 * For serializing a signature
 */
object ProofSerializer extends GjalSerializer[Proof[_]] {

  def serialize(obj: Proof[_], w: Writer): Unit =
    obj match {
      case obj: SignatureCurve25519 =>
        w.put(PublicKeyPropositionCurve25519.typePrefix)
        SignatureCurve25519Serializer.serialize(obj, w)

      case obj: ThresholdSignatureCurve25519 =>
        w.put(ThresholdPropositionCurve25519.typePrefix)
        ThresholdSignatureCurve25519Serializer.serialize(obj, w)
    }

  def parse(r: Reader): Proof[_ <: Proposition] =
    r.getByte() match {
      case PublicKeyPropositionCurve25519.typePrefix => SignatureCurve25519Serializer.parse(r)
      case ThresholdPropositionCurve25519.typePrefix => ThresholdSignatureCurve25519Serializer.parse(r)
    }
}
