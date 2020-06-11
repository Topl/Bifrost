package bifrost.modifier

import com.google.common.primitives.Ints

case class ModifierId(hashBytes: Array[Byte]) {

  override def hashCode: Int = Ints.fromByteArray(hashBytes)

  override def equals(o: Any): Boolean = {
    o.isInstanceOf[ModifierId] &&
    java.util.Arrays.equals(hashBytes, o.asInstanceOf[ModifierId].hashBytes)
  }
}

object ModifierId {

}
