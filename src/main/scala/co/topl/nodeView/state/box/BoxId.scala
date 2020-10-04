package co.topl.nodeView.state.box

import co.topl.crypto.FastCryptographicHash
import com.google.common.primitives.Ints
import scorex.crypto.encode.Base58

import scala.util.{ Failure, Success }

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

  def apply(id: String): BoxId = {
    Base58.decode(id) match {
      case Success(id) => new BoxId(id)
      case Failure(ex) => throw ex
    }
  }
}
