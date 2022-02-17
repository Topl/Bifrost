package co.topl.codecs.binary.legacy.attestation

import co.topl.attestation.SignatureEd25519
import co.topl.codecs.binary.legacy.{BifrostSerializer, Writer}
import co.topl.codecs.binary.legacy.Reader
import co.topl.crypto.Signature
import co.topl.crypto.signing.Ed25519

object SignatureEd25519Serializer extends BifrostSerializer[SignatureEd25519] {

  override def serialize(obj: SignatureEd25519, w: Writer): Unit =
    w.putBytes(obj.sigBytes.value)

  override def parse(r: Reader): SignatureEd25519 = {
    val sigBytes = r.getBytes(Ed25519.instance.SignatureLength)
    SignatureEd25519(Signature(sigBytes))
  }
}
