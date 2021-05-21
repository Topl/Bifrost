package modifier

import attestation.Evidence
import cats.implicits._
import co.topl.crypto.hash.Blake2b256
import co.topl.crypto.hash.digest.Digest32
import co.topl.utils.StringTypes.Base58String
import co.topl.utils.encode.Base58
import co.topl.utils.StringTypes.implicits._
import com.google.common.primitives.{Ints, Longs}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}

import scala.util.{Failure, Success}

/**
 * The ID for a [[Box]]. It is a 32 byte identifier
 * @param hashBytes the bytes used to create the id
 */
case class BoxId(hashBytes: Array[Byte]) {

  override def hashCode: Int = Ints.fromByteArray(hashBytes)

  override def equals(obj: Any): Boolean = obj match {
    case obj: BoxId => obj.hashBytes sameElements hashBytes
    case _          => false
  }

  override def toString: String = Base58.encode(hashBytes).show
}

object BoxId {

  val size: Int = Digest32.size // boxId is a 32 byte identifier

  def apply[T](box: Box): BoxId = idFromEviNonce(box.evidence, box.nonce)

  def apply(id: String): BoxId = {
    val idBytes = Base58.decode(Base58String.unsafe(id))

    require(idBytes.length == BoxId.size, s"Invalid size for BoxId")
    new BoxId(idBytes)
  }

  def idFromEviNonce(evidence: Evidence, nonce: Long): BoxId =
    BoxId(Blake2b256.hash(evidence.bytes ++ Longs.toByteArray(nonce)).value)

  implicit val jsonEncoder: Encoder[BoxId] = (id: BoxId) => id.toString.asJson
  implicit val jsonKeyEncoder: KeyEncoder[BoxId] = (id: BoxId) => id.toString
  implicit val jsonDecoder: Decoder[BoxId] = Decoder.decodeString.map(apply)
  implicit val jsonKeyDecoder: KeyDecoder[BoxId] = (id: String) => Some(apply(id))
}
