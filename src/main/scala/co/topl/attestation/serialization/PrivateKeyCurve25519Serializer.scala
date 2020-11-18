package co.topl.attestation.serialization

import co.topl.attestation.PrivateKeyCurve25519
import co.topl.utils.serialization.{BifrostSerializer, Reader, Writer}
import scorex.crypto.signatures.{Curve25519, PrivateKey, PublicKey}

object PrivateKeyCurve25519Serializer extends BifrostSerializer[PrivateKeyCurve25519] {

  override def serialize(obj: PrivateKeyCurve25519, w: Writer): Unit = {
    /* privKeyBytes: Array[Byte] */
    w.putBytes(obj.privKeyBytes)

    /* publicKeyBytes: Array[Byte] */
    w.putBytes(obj.publicKeyBytes)
  }

  override def parse(r: Reader): PrivateKeyCurve25519 = {
    PrivateKeyCurve25519(PrivateKey @@ r.getBytes(Curve25519.KeyLength), PublicKey @@ r.getBytes(Curve25519.KeyLength))
  }
}
