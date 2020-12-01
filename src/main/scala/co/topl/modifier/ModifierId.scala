package co.topl.modifier

import com.google.common.primitives.Ints
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}
import scorex.crypto.hash.Blake2b256
import scorex.util.encode.Base58

import scala.util.{Failure, Success, Try}

case class ModifierId(hashBytes: Array[Byte]) {

  require(hashBytes.length == ModifierId.size, s"Invalid size for ModifierId")

  override def hashCode: Int = Ints.fromByteArray(hashBytes)

  override def equals(o: Any): Boolean = {
    o.isInstanceOf[ModifierId] &&
      java.util.Arrays.equals(hashBytes, o.asInstanceOf[ModifierId].hashBytes)
  }

  override def toString: String = Base58.encode(hashBytes)
}

object ModifierId {
  val size: Int = Blake2b256.DigestSize // boxId is a 32 byte identifier

  implicit val ord: Ordering[ModifierId] = Ordering.by(_.toString)

  def apply(encodedSig: String): ModifierId =
    Base58.decode(encodedSig) match {
      case Success(sig) => new ModifierId(sig)
      case Failure(ex)  => throw ex
    }

  implicit val jsonEncoder: Encoder[ModifierId] =
    (id: ModifierId) => id.toString.asJson

  implicit val jsonDecoder: Decoder[ModifierId] =
    Decoder.decodeString.emapTry { id => Try(ModifierId(id)) }

  implicit val jsonKeyEncoder: KeyEncoder[ModifierId] =
    ( id: ModifierId ) => id.toString

  implicit val jsonKeyDecoder: KeyDecoder[ModifierId] =
    ( id: String ) => Some(ModifierId(id))
}
