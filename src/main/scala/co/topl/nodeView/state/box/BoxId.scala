package co.topl.nodeView.state.box

import com.google.common.primitives.Ints
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}
import scorex.util.encode.Base58
import scorex.crypto.hash.Blake2b256

import scala.util.{Failure, Success, Try}

case class BoxId (hashBytes: Array[Byte]) {
  require(hashBytes.length == BoxId.size, s"Invalid size for BoxId")

  override def hashCode: Int = Ints.fromByteArray(hashBytes)

  override def equals(o: Any): Boolean = {
    o.isInstanceOf[BoxId] &&
      java.util.Arrays.equals(hashBytes, o.asInstanceOf[BoxId].hashBytes)
  }

  override def toString: String = Base58.encode(hashBytes)
}

object BoxId {
  val size: Int = Blake2b256.DigestSize // boxId is a 32 byte identifier

  def apply(id: String): BoxId = {
    Base58.decode(id) match {
      case Success(id) => new BoxId(id)
      case Failure(ex) => throw ex
    }
  }

  implicit val jsonEncoder: Encoder[BoxId] =
    (id: BoxId) => id.toString.asJson

  implicit val jsonDecoder: Decoder[BoxId] =
    Decoder.decodeString.emapTry { id => Try(BoxId(id)) }

  implicit val jsonKeyEncoder: KeyEncoder[BoxId] =
    ( id: BoxId ) => id.toString

  implicit val jsonKeyDecoder: KeyDecoder[BoxId] =
    ( id: String ) => Some(BoxId(id))
}
