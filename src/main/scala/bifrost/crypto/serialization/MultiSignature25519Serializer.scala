package bifrost.crypto.serialization

import bifrost.crypto.{MultiSignature25519, Signature25519}
import bifrost.utils.Extensions._
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}

object MultiSignature25519Serializer extends BifrostSerializer[MultiSignature25519] {

  override def serialize(obj: MultiSignature25519, w: Writer): Unit = {
    /* signatureSet: Set[Signature25519] */
    w.putUInt(obj.signatureSet.size)
    obj.signatureSet.foreach(sig => Signature25519Serializer.serialize(sig, w))
  }

  override def parse(r: Reader): MultiSignature25519 = {
    val signatureSetLength: Int = r.getUInt().toIntExact
    val signatureSet: Set[Signature25519] = (0 until signatureSetLength).map(_ => Signature25519Serializer.parse(r)).toSet

    MultiSignature25519(signatureSet)
  }
}
