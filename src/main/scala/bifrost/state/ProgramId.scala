package bifrost.state

import java.util.UUID

import bifrost.crypto.FastCryptographicHash
import bifrost.modifier.ModifierId
import com.google.common.primitives.Ints
import scorex.crypto.encode.Base58

case class ProgramId (hashBytes: Array[Byte]) {

  override def hashCode: Int = Ints.fromByteArray(hashBytes)

  override def equals(o: Any): Boolean = {
    o.isInstanceOf[ModifierId] &&
      java.util.Arrays.equals(hashBytes, o.asInstanceOf[ModifierId].hashBytes)
  }

  override def toString: String = Base58.encode(hashBytes)
}

object ProgramId {


  def apply (): ProgramId = {

    val key = UUID.randomUUID()
    val keyBytes = BigInt(key.getMostSignificantBits).toByteArray ++ BigInt(key.getLeastSignificantBits).toByteArray

  new ProgramId(FastCryptographicHash(keyBytes))

  }
}
