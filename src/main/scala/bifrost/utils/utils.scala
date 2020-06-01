package bifrost

import bifrost.utils.encode.Base16
import supertagged.TaggedType

package object utils {

  object ModifierId extends TaggedType[String]
  type ModifierId = ModifierId.Type

  def bytesToId(bytes: Array[Byte]): ModifierId = ModifierId @@ Base16.encode(bytes)

  def idToBytes(id: ModifierId): Array[Byte] = Base16.decode(id).get

  implicit class ModifierIdOps(val m: ModifierId) extends AnyVal {
    @inline def toBytes: Array[Byte] = idToBytes(m)
  }

  implicit class ByteArrayOps(val b: Array[Byte]) extends AnyVal  {
    @inline def toModifierId: ModifierId = bytesToId(b)
  }
}
