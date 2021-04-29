package attestation.serialization

import attestation.SignatureCurve25519
import scorex.crypto.signatures.Signature
import utils.serialization.{GjalSerializer, Reader, Writer}

/**
 * For serializing a Signature for a PublicKeyProposition
 */
object SignatureCurve25519Serializer extends GjalSerializer[SignatureCurve25519] {

  override def serialize(obj: SignatureCurve25519, w: Writer): Unit =
    w.putBytes(obj.sigBytes)

  override def parse(r: Reader): SignatureCurve25519 = {
    val sigBytes = r.getBytes(SignatureCurve25519.signatureSize)
    SignatureCurve25519(Signature @@ sigBytes)
  }
}
