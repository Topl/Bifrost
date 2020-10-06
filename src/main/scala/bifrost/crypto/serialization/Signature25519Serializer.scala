package bifrost.crypto.serialization

import bifrost.crypto.Signature25519
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}
import scorex.crypto.signatures.{Curve25519, Signature}

object Signature25519Serializer extends BifrostSerializer[Signature25519] {

  override def serialize(obj: Signature25519, w: Writer): Unit = w.putBytes(obj.signature)

  override def parse(r: Reader): Signature25519 = Signature25519(Signature @@ r.getBytes(Curve25519.SignatureLength))
}
