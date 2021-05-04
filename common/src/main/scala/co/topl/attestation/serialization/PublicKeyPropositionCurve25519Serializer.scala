package co.topl.attestation.serialization

import co.topl.attestation.PublicKeyPropositionCurve25519
import co.topl.crypto.signatures.{Curve25519, PublicKey}
import co.topl.utils.serialization.{BifrostSerializer, Reader, Writer}
import co.topl.utils.AsBytes.implicits._

object PublicKeyPropositionCurve25519Serializer extends BifrostSerializer[PublicKeyPropositionCurve25519] {

  override def serialize(obj: PublicKeyPropositionCurve25519, w: Writer): Unit =
    w.putBytes(obj.pubKeyBytes)

  override def parse(r: Reader): PublicKeyPropositionCurve25519 = {
    val proposition = r.getBytes(Curve25519.KeyLength)
    PublicKeyPropositionCurve25519(PublicKey(proposition))
  }
}
