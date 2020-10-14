package co.topl.nodeView.state.box

import com.google.common.primitives.Ints
import scorex.util.encode.Base58

case class BoxId (hashBytes: Array[Byte]) {
  override def hashCode: Int = Ints.fromByteArray(hashBytes)

  override def equals(o: Any): Boolean = {
    o.isInstanceOf[BoxId] &&
      java.util.Arrays.equals(hashBytes, o.asInstanceOf[BoxId].hashBytes)
  }

  override def toString: String = Base58.encode(hashBytes)
}

object BoxId {
  val size: Int = 32 // boxId is a 32 byte identifier
}
