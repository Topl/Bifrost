package co.topl.attestation.serialization

import co.topl.attestation.SignatureCurve25519
import co.topl.utils.serialization.{BifrostSerializer, Reader, Writer}
import co.topl.crypto.signatures.Signature

object SignatureCurve25519Serializer extends BifrostSerializer[SignatureCurve25519] {

  override def serialize(obj: SignatureCurve25519, w: Writer): Unit =
    w.putBytes(obj.sig.toBytes)

  override def parse(r: Reader): SignatureCurve25519 = {
    val sigBytes = r.getBytes(SignatureCurve25519.signatureSize)
    SignatureCurve25519(Signature(sigBytes))
  }
}
