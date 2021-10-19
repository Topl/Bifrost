package co.topl.modifier.box

import cats.implicits._
import co.topl.attestation.Evidence
import co.topl.crypto.hash.blake2b256
import co.topl.crypto.hash.digest.Digest32
import co.topl.crypto.hash.implicits._
import co.topl.utils.IdiomaticScalaTransition.implicits.toValidatedOps
import co.topl.utils.StringDataTypes.Base58Data
import co.topl.utils.StringDataTypes.implicits._
import co.topl.utils.codecs.binary.implicits._
import co.topl.utils.codecs.json.codecs._
import com.google.common.primitives.{Ints, Longs}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}

case class BoxId(hash: Digest32) {

  override def hashCode: Int = Ints.fromByteArray(hash.value)

  override def equals(obj: Any): Boolean = obj match {
    case obj: BoxId => obj.hash === hash
    case _          => false
  }

  override def toString: String = hash.encodeAsBase58.show
}

object BoxId {

  val size: Int = Digest32.size // boxId is a 32 byte identifier

  def apply[T](box: Box[T]): BoxId = idFromEviNonce(box.evidence, box.nonce)

  def apply(bytes: Array[Byte]): BoxId =
    Digest32.validated(bytes).map(BoxId(_)).getOrThrow()

  // requires a dummy implicit to be different from BoxId(Digest32) after type erasure
  def apply(id: Base58Data)(implicit dummy: DummyImplicit): BoxId = apply(id.value)

  def idFromEviNonce(evidence: Evidence, nonce: Box.Nonce): BoxId =
    BoxId(blake2b256.hash(evidence.bytes ++ Longs.toByteArray(nonce)))

  implicit val jsonEncoder: Encoder[BoxId] = (id: BoxId) => id.toString.asJson
  implicit val jsonKeyEncoder: KeyEncoder[BoxId] = (id: BoxId) => id.toString
  implicit val jsonDecoder: Decoder[BoxId] = Decoder[Base58Data].map(apply)
  implicit val jsonKeyDecoder: KeyDecoder[BoxId] = KeyDecoder[Base58Data].map(apply)
}
