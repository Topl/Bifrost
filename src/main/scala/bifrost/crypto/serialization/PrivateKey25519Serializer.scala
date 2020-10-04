package bifrost.crypto.serialization

import bifrost.crypto.PrivateKey25519
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}
import scorex.crypto.signatures.{Curve25519, PrivateKey, PublicKey}

object PrivateKey25519Serializer extends BifrostSerializer[PrivateKey25519] {

  override def serialize(obj: PrivateKey25519, w: Writer): Unit = {
    /* privKeyBytes: Array[Byte] */
    w.putBytes(obj.privKeyBytes)

    /* publicKeyBytes: Array[Byte] */
    w.putBytes(obj.publicKeyBytes)
  }

  override def parse(r: Reader): PrivateKey25519 = {
    PrivateKey25519(PrivateKey @@ r.getBytes(Curve25519.KeyLength), PublicKey @@ r.getBytes(Curve25519.KeyLength))
  }
}
