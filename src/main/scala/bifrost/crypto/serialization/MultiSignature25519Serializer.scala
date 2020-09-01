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

  //  TODO: Jing - remove
  //
  //  override def toBytes(obj: MultiSignature25519): Array[Byte] =
  //    Ints.toByteArray(obj.signatureSet.size) ++
  //      obj
  //        .signatureSet
  //        .foldLeft(Array[Byte]())((total, sig) => total ++ sig.signature)
  //
  //  override def parseBytes(bytes: Array[Byte]): Try[MultiSignature25519] = Try {
  //    val numSignatures = Ints.fromByteArray(bytes.take(Ints.BYTES))
  //    val signatureSet: Set[Signature25519] = (0 until numSignatures).map {
  //      i =>
  //        Signature25519(bytes.slice(Ints.BYTES + i * MultiSignature25519.SignatureSize,
  //                                   Ints.BYTES + (i + 1) * MultiSignature25519.SignatureSize))
  //    }.toSet
  //
  //    MultiSignature25519(signatureSet)
  //  }
}
