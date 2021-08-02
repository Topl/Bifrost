package co.topl.attestation.keyManagement.derivedKeys

import co.topl.utils.SizedByteCollection
import co.topl.utils.SizedByteCollection.Types.ByteVector32
import co.topl.utils.SizedByteCollection.implicits._
import co.topl.utils.serialization.{BifrostSerializer, Reader, Writer}
import scodec.bits.ByteOrdering

object ExtendedPrivateKeyEd25519Serializer extends BifrostSerializer[ExtendedPrivateKeyEd25519] {

  override def serialize(obj: ExtendedPrivateKeyEd25519, w: Writer): Unit = {
    w.putBytes(obj.leftKey.toArray)
    w.putBytes(obj.rightKey.toArray)
    w.putBytes(obj.chainCode.toArray)
  }

  override def parse(r: Reader): ExtendedPrivateKeyEd25519 = {
    val leftKey = r.getBytes(ByteVector32.size)
    val rightKey = r.getBytes(ByteVector32.size)
    val chainCode = r.getBytes(ByteVector32.size)

    ExtendedPrivateKeyEd25519(
      SizedByteCollection[ByteVector32].fit(leftKey, ByteOrdering.LittleEndian),
      SizedByteCollection[ByteVector32].fit(rightKey, ByteOrdering.LittleEndian),
      SizedByteCollection[ByteVector32].fit(chainCode, ByteOrdering.LittleEndian),
      // Branden TODO: figure out how to serialize/deserialize the derivation path
      Seq()
    )
  }
}
