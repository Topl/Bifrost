package co.topl.modifier.box

import co.topl.attestation.Evidence
import co.topl.crypto.Implicits._
import co.topl.crypto.hash.Digest32
import co.topl.utils.blake2b256
import co.topl.utils.encode.Base58
import com.google.common.primitives.{Ints, Longs}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}

import scala.util.{Failure, Success}

case class BoxId(hashBytes: Array[Byte]) {

  override def hashCode: Int = Ints.fromByteArray(hashBytes)

  override def equals(obj: Any): Boolean = obj match {
    case obj: BoxId => obj.hashBytes sameElements hashBytes
    case _          => false
  }

  override def toString: String = Base58.encode(hashBytes)
}

object BoxId {

  val size: Int = Digest32.size // boxId is a 32 byte identifier

  def apply[T](box: Box[T]): BoxId = idFromEviNonce(box.evidence, box.nonce)

  def apply(id: String): BoxId =
    Base58.decode(id) match {
      case Success(id) =>
        require(id.length == BoxId.size, s"Invalid size for BoxId")
        new BoxId(id)

      case Failure(ex) => throw ex
    }

  def idFromEviNonce(evidence: Evidence, nonce: Box.Nonce): BoxId =
    BoxId(blake2b256(evidence.bytes ++ Longs.toByteArray(nonce)))

  implicit val jsonEncoder: Encoder[BoxId] = (id: BoxId) => id.toString.asJson
  implicit val jsonKeyEncoder: KeyEncoder[BoxId] = (id: BoxId) => id.toString
  implicit val jsonDecoder: Decoder[BoxId] = Decoder.decodeString.map(apply)
  implicit val jsonKeyDecoder: KeyDecoder[BoxId] = (id: String) => Some(apply(id))
}
