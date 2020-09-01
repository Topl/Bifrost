package bifrost.crypto.serialization

import bifrost.crypto.PrivateKey25519
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}
import scorex.crypto.signatures.Curve25519

object PrivateKey25519Serializer extends BifrostSerializer[PrivateKey25519] {

  override def serialize(obj: PrivateKey25519, w: Writer): Unit = {
    /* privKeyBytes: Array[Byte] */
    w.putBytes(obj.privKeyBytes)

    /* publicKeyBytes: Array[Byte] */
    w.putBytes(obj.publicKeyBytes)
  }

  override def parse(r: Reader): PrivateKey25519 = {
    PrivateKey25519(r.getBytes(Curve25519.KeyLength), r.getBytes(Curve25519.KeyLength))
  }

  //  TODO: Jing - remove
  //
  //  override def toBytes(obj: PrivateKey25519): Array[Byte] = Bytes.concat(obj.privKeyBytes, obj.publicKeyBytes)
  //
  //  override def parseBytes(bytes: Array[Byte]): Try[PrivateKey25519] = Try {
  //    PrivateKey25519(bytes.slice(0, 32), bytes.slice(32, 64))
  //  }
}
