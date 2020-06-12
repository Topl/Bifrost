package bifrost

import bifrost.modifier.ModifierId
import scorex.crypto.encode.Base58

package object utils {

  def bytesToId(bytes: Array[Byte]): ModifierId = ModifierId(bytes)

  def idToBytes(id: ModifierId): Array[Byte] = Base58.decode(id).get
  
  def idToString(id: ModifierId): String = Base58.encode(id.hashBytes)
}
