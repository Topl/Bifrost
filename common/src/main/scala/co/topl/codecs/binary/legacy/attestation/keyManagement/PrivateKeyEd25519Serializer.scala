package co.topl.codecs.binary.legacy.attestation.keyManagement

import co.topl.attestation.keyManagement.PrivateKeyEd25519
import co.topl.codecs.binary.legacy.{BifrostSerializer, Reader, Writer}
import co.topl.crypto.signing.Ed25519
import co.topl.crypto.{PrivateKey, PublicKey}

object PrivateKeyEd25519Serializer extends BifrostSerializer[PrivateKeyEd25519] {

  override def serialize(obj: PrivateKeyEd25519, w: Writer): Unit = {
    /* privKeyBytes: Array[Byte] */
    w.putBytes(obj.privateKey.value)

    /* publicKeyBytes: Array[Byte] */
    w.putBytes(obj.publicKey.value)
  }

  override def parse(r: Reader): PrivateKeyEd25519 =
    PrivateKeyEd25519(
      PrivateKey(r.getBytes(Ed25519.instance.KeyLength)),
      PublicKey(r.getBytes(Ed25519.instance.KeyLength))
    )
}
