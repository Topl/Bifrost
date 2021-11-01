package co.topl.attestation.serialization

import co.topl.attestation.SignatureEd25519
import co.topl.crypto.Signature
import co.topl.crypto.signing.Ed25519
import co.topl.utils.serialization.{BifrostSerializer, Reader, Writer}

object SignatureEd25519Serializer extends BifrostSerializer[SignatureEd25519] {

  override def serialize(obj: SignatureEd25519, w: Writer): Unit =
    w.putBytes(obj.sigBytes.value)

  override def parse(r: Reader): SignatureEd25519 = {
    val sigBytes = r.getBytes(Ed25519.instance.SignatureLength)
    SignatureEd25519(Signature(sigBytes))
  }
}
