package co.topl.modifier.box

import cats.data.NonEmptyChain
import cats.data.Validated.{Invalid, Valid}
import cats.implicits._
import co.topl.attestation.Evidence
import co.topl.crypto.hash.implicits._
import co.topl.crypto.hash.Blake2b256
import co.topl.crypto.hash.digest.{Digest32, InvalidDigestFailure}
import co.topl.utils.codecs.implicits._
import co.topl.utils.StringTypes.implicits._
import co.topl.utils.StringTypes.Base58String
import co.topl.utils.encode.Base58
import co.topl.utils.IdiomaticScalaTransition.implicits.toValidatedOps
import com.google.common.primitives.{Ints, Longs}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}

case class BoxId(hash: Digest32) {

  override def hashCode: Int = Ints.fromByteArray(hash.value)

  override def equals(obj: Any): Boolean = obj match {
    case obj: BoxId => obj.hash === hash
    case _          => false
  }

  override def toString: String = hash.infalliblyDecodeTo[Base58String].show
}

object BoxId {

  val size: Int = Digest32.size // boxId is a 32 byte identifier

  def apply[T](box: Box[T]): BoxId = idFromEviNonce(box.evidence, box.nonce)

  def apply(id: Base58String)(implicit dummy: DummyImplicit): BoxId =
    id.decodeTo[InvalidDigestFailure, Digest32].map(BoxId(_)).getOrThrow()

  def apply(bytes: Array[Byte]): BoxId =
    Digest32.validated(bytes).map(BoxId(_)).getOrThrow()

  def idFromEviNonce(evidence: Evidence, nonce: Box.Nonce): BoxId =
    BoxId(Blake2b256.hash(evidence.bytes ++ Longs.toByteArray(nonce)))

  implicit val jsonEncoder: Encoder[BoxId] = (id: BoxId) => id.toString.asJson
  implicit val jsonKeyEncoder: KeyEncoder[BoxId] = (id: BoxId) => id.toString

  implicit val jsonDecoder: Decoder[BoxId] =
    Decoder.decodeString
      .emap(Base58String.validated(_).leftMap(_ => "Value is not Base 58").toEither)
      .map(apply)

  implicit val jsonKeyDecoder: KeyDecoder[BoxId] = (id: String) => Base58String.validated(id).map(apply).toOption
}
