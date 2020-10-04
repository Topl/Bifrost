package co.topl.modifier

import com.google.common.primitives.Ints
import scorex.crypto.encode.Base58
import co.topl.crypto.FastCryptographicHash

import scala.util.{Failure, Success}

case class ModifierId(hashBytes: Array[Byte]) {

  require(hashBytes.length == FastCryptographicHash.DigestSize, s"Invalid size for ModifierId")

  override def hashCode: Int = Ints.fromByteArray(hashBytes)

  override def equals(o: Any): Boolean = {
    o.isInstanceOf[ModifierId] &&
      java.util.Arrays.equals(hashBytes, o.asInstanceOf[ModifierId].hashBytes)
  }

  override def toString: String = Base58.encode(hashBytes)
}

object ModifierId {
  implicit val ord: Ordering[ModifierId] = Ordering.by(_.toString)

  def apply(encodedSig: String): ModifierId =
    Base58.decode(encodedSig) match {
      case Success(sig) => new ModifierId(sig)
      case Failure(ex)  => throw ex
    }

}
