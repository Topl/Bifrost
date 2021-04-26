package attestation.serialization

import attestation.PublicKeyPropositionCurve25519
import co.topl.crypto.signatures.{Curve25519, PublicKey}
import utils.serialization.{GjalSerializer, Reader, Writer}

/**
  * For serializing a PublicKeyProposition
  */
object PublicKeyPropositionCurve25519Serializer extends GjalSerializer[PublicKeyPropositionCurve25519] {

  override def serialize(obj: PublicKeyPropositionCurve25519, w: Writer): Unit = {
    w.putBytes(obj.pubKeyBytes.toBytes)
  }

  override def parse(r: Reader): PublicKeyPropositionCurve25519 = {
    val proposition = r.getBytes(Curve25519.KeyLength)
    PublicKeyPropositionCurve25519(PublicKey(proposition))
  }
}
