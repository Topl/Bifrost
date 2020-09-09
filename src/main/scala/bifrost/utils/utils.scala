package bifrost

import bifrost.modifier.ModifierId

// TODO: Jing - This is no longer needed, we could use ModifierId.hashBytes instead
package object utils {

  def bytesToId(bytes: Array[Byte]): ModifierId = ModifierId(bytes)

  def idToBytes(id: ModifierId): Array[Byte] = id.hashBytes

  def idToString(id: ModifierId): String = id.toString
}
