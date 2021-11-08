package co.topl.codecs.binary.legacy.attestation.keyManagement

import co.topl.attestation.keyManagement.PrivateKeyCurve25519
import co.topl.crypto.signatures.Curve25519
import co.topl.crypto.{PrivateKey, PublicKey}
import co.topl.codecs.binary.legacy.{BifrostSerializer, Reader, Writer}

object PrivateKeyCurve25519Serializer extends BifrostSerializer[PrivateKeyCurve25519] {

  override def serialize(obj: PrivateKeyCurve25519, w: Writer): Unit = {
    /* : Array[Byte] */
    w.putBytes(obj.privateKey.value)

    /* publicKeyBytes: Array[Byte] */
    w.putBytes(obj.publicKey.value)
  }

  override def parse(r: Reader): PrivateKeyCurve25519 =
    new PrivateKeyCurve25519(PrivateKey(r.getBytes(Curve25519.KeyLength)), PublicKey(r.getBytes(Curve25519.KeyLength)))

}
