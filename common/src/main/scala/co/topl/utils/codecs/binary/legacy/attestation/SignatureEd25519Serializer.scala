package co.topl.utils.codecs.binary.legacy.attestation

import co.topl.attestation.SignatureEd25519
import co.topl.crypto.signatures.Signature
import co.topl.utils.codecs.binary.legacy.{BifrostSerializer, Reader, Writer}

object SignatureEd25519Serializer extends BifrostSerializer[SignatureEd25519] {

  override def serialize(obj: SignatureEd25519, w: Writer): Unit =
    w.putBytes(obj.sigBytes.value)

  override def parse(r: Reader): SignatureEd25519 = {
    val sigBytes = r.getBytes(SignatureEd25519.signatureSize)
    SignatureEd25519(Signature(sigBytes))
  }
}
