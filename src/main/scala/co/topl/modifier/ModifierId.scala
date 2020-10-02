package co.topl.modifier

import com.google.common.primitives.Ints
import scorex.crypto.encode.Base58

case class ModifierId(hashBytes: Array[Byte]) {

  override def hashCode: Int = Ints.fromByteArray(hashBytes)

  override def equals(o: Any): Boolean = {
    o.isInstanceOf[ModifierId] &&
      java.util.Arrays.equals(hashBytes, o.asInstanceOf[ModifierId].hashBytes)
  }

  override def toString: String = Base58.encode(hashBytes)
}

object ModifierId {
  implicit val ord: Ordering[ModifierId] = Ordering.by(_.toString)

}
