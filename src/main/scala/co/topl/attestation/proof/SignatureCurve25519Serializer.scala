package co.topl.attestation.proof

import co.topl.utils.serialization.{BifrostSerializer, Reader, Writer}
import scorex.crypto.signatures.Signature

object SignatureCurve25519Serializer extends BifrostSerializer[SignatureCurve25519] {

  override def serialize(obj: SignatureCurve25519, w: Writer): Unit =
    w.putBytes(obj.sigBytes)

  override def parse(r: Reader): SignatureCurve25519 = {
    val sigBytes = r.getBytes(SignatureCurve25519.SignatureSize)

    SignatureCurve25519(Signature @@ sigBytes)
  }
}
