package attestation.serialization

/**
 * For serializing a Signature for a PublicKeyProposition
 */
object SignatureCurve25519Serializer extends GjalSerializer[SignatureCurve25519] {

  override def serialize(obj: SignatureCurve25519, w: Writer): Unit =
    w.putBytes(obj.signature.value)

  override def parse(r: Reader): SignatureCurve25519 = {
    val sigBytes = r.getBytes(SignatureCurve25519.signatureSize)
    SignatureCurve25519(Signature(sigBytes))
  }
}
