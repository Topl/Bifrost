package bifrost

import bifrost.modifier.ModifierId
import bifrost.utils.encode.Base16
import scorex.crypto.encode.Base58

package object utils {

  def bytesToId(bytes: Array[Byte]): ModifierId = ModifierId @@ Base16.encode(bytes)

  def idToBytes(id: ModifierId): Array[Byte] = Base16.decode(id).get
  
  def idToString(id: ModifierId): String = Base58.encode(id.hashBytes)
}
