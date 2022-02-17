package co.topl.codecs.binary.legacy.attestation

import co.topl.attestation.PublicKeyPropositionCurve25519
import co.topl.codecs.binary.legacy.{BifrostSerializer, Reader, Writer}
import co.topl.crypto.PublicKey
import co.topl.crypto.signing.Curve25519

object PublicKeyPropositionCurve25519Serializer extends BifrostSerializer[PublicKeyPropositionCurve25519] {

  override def serialize(obj: PublicKeyPropositionCurve25519, w: Writer): Unit =
    w.putBytes(obj.pubKeyBytes.value)

  override def parse(r: Reader): PublicKeyPropositionCurve25519 = {
    val proposition = r.getBytes(Curve25519.instance.KeyLength)
    PublicKeyPropositionCurve25519(PublicKey(proposition))
  }
}
