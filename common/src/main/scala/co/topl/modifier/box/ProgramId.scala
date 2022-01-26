package co.topl.modifier.box

import co.topl.codecs.binary.legacy.modifier.box.ProgramIdSerializer
import co.topl.codecs.binary.legacy.{BifrostSerializer, BytesSerializable}
import co.topl.crypto.hash.blake2b256
import co.topl.crypto.hash.digest.Digest32
import co.topl.utils.StringDataTypes.Base58Data
import co.topl.utils.encode.Base58
import com.google.common.primitives.Ints

case class ProgramId(hashBytes: Array[Byte]) extends BytesSerializable {

  override type M = ProgramId
  override def serializer: BifrostSerializer[ProgramId] = ProgramIdSerializer

  override def equals(obj: Any): Boolean = obj match {
    case obj: ProgramId => obj.hashBytes sameElements hashBytes
    case _              => false
  }

  override def toString: String = Base58.encode(hashBytes)

  override def hashCode: Int = Ints.fromByteArray(hashBytes)
}

object ProgramId {

  val size: Int = Digest32.size; // number of bytes in identifier,

  def apply(id: Base58Data): ProgramId = {
    val idBytes = id.value
    require(idBytes.length == ProgramId.size, s"Invalid size for ProgramId")
    new ProgramId(idBytes)
  }

  def create(seed: Array[Byte]): ProgramId =
    new ProgramId(blake2b256.hash(seed).value)

}
