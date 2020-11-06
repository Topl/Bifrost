package co.topl.crypto.serialization

import co.topl.crypto.Signature25519
import co.topl.utils.serialization.{BifrostSerializer, Reader, Writer}
import scorex.crypto.signatures.{Curve25519, Signature}

object Signature25519Serializer extends BifrostSerializer[Signature25519] {

  override def serialize(obj: Signature25519, w: Writer): Unit = w.putBytes(obj.signature)

  override def parse(r: Reader): Signature25519 = Signature25519(Signature @@ r.getBytes(Signature25519.SignatureSize))
}
