package co.topl.modifier.box

import cats.implicits._
import co.topl.attestation.Evidence
import co.topl.crypto.hash.{blake2b256, Digest32}
import co.topl.utils.AsBytes.implicits._
import co.topl.utils.StringTypes.Base58String
import co.topl.utils.StringTypes.implicits._
import co.topl.utils.encode.Base58
import com.google.common.primitives.{Ints, Longs}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}

case class BoxId(hashBytes: Array[Byte]) {

  override def hashCode: Int = Ints.fromByteArray(hashBytes)

  override def equals(obj: Any): Boolean = obj match {
    case obj: BoxId => obj.hashBytes sameElements hashBytes
    case _          => false
  }

  override def toString: String = Base58.encode(hashBytes).map(_.show).getOrElse("")
}

object BoxId {

  val size: Int = Digest32.size // boxId is a 32 byte identifier

  def apply[T](box: Box[T]): BoxId = idFromEviNonce(box.evidence, box.nonce)

  def apply(id: Base58String): BoxId =
    Base58.decode(id) match {
      case Right(id) =>
        require(id.length == BoxId.size, s"Invalid size for BoxId")
        new BoxId(id)
      case Left(_) => throw new Exception("Failed to decode value to bytes.")
    }

  def idFromEviNonce(evidence: Evidence, nonce: Box.Nonce): BoxId =
    BoxId(blake2b256(evidence.bytes ++ Longs.toByteArray(nonce)).asBytes)

  implicit val jsonEncoder: Encoder[BoxId] = (id: BoxId) => id.toString.asJson
  implicit val jsonKeyEncoder: KeyEncoder[BoxId] = (id: BoxId) => id.toString

  implicit val jsonDecoder: Decoder[BoxId] =
    Decoder.decodeString
      .emap(Base58String.validated(_).leftMap(_ => "Value is not Base 58"))
      .map(apply)

  implicit val jsonKeyDecoder: KeyDecoder[BoxId] = (id: String) => Base58String.validated(id).map(apply).toOption
}
