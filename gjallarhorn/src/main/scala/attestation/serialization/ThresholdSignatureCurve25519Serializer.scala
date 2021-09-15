package attestation.serialization

/**
 * For serializing a ThresholdSignature
 */
object ThresholdSignatureCurve25519Serializer extends GjalSerializer[ThresholdSignatureCurve25519] {

  override def serialize(obj: ThresholdSignatureCurve25519, w: Writer): Unit = {
    /* signatureSet: Set[Signature25519] */
    w.putUInt(obj.signatures.size)
    obj.signatures.foreach(sig => SignatureCurve25519Serializer.serialize(sig, w))
  }

  override def parse(r: Reader): ThresholdSignatureCurve25519 = {
    val signatureSetLength: Int = r.getUInt().toIntExact
    val signatureSet: Set[SignatureCurve25519] =
      (0 until signatureSetLength).map(_ => SignatureCurve25519Serializer.parse(r)).toSet

    ThresholdSignatureCurve25519(signatureSet)
  }
}
