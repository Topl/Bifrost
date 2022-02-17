package co.topl.codecs.binary.legacy.attestation.keyManagement

import co.topl.attestation.keyManagement.PrivateKeyCurve25519
import co.topl.codecs.binary.legacy.{BifrostSerializer, Reader, Writer}
import co.topl.crypto.signing.Curve25519
import co.topl.crypto.{PrivateKey, PublicKey}

object PrivateKeyCurve25519Serializer extends BifrostSerializer[PrivateKeyCurve25519] {

  override def serialize(obj: PrivateKeyCurve25519, w: Writer): Unit = {
    /* : Array[Byte] */
    w.putBytes(obj.privateKey.value)

    /* publicKeyBytes: Array[Byte] */
    w.putBytes(obj.publicKey.value)
  }

  override def parse(r: Reader): PrivateKeyCurve25519 =
    new PrivateKeyCurve25519(
      PrivateKey(r.getBytes(Curve25519.instance.KeyLength)),
      PublicKey(r.getBytes(Curve25519.instance.KeyLength))
    )

}
