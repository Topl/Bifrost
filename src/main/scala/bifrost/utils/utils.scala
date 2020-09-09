package bifrost

import bifrost.modifier.ModifierId

// TODO: Jing - This is no longer needed, we could use ModifierId.hashBytes instead
package object utils {

  @deprecated
  def bytesToId(bytes: Array[Byte]): ModifierId = ModifierId(bytes)

  @deprecated
  def idToBytes(id: ModifierId): Array[Byte] = id.hashBytes

  @deprecated
  def idToString(id: ModifierId): String = id.toString
}
