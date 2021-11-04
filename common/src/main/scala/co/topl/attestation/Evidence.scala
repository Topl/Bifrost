package co.topl.attestation

import cats.implicits.toShow
import co.topl.crypto.hash.digest.Digest
import co.topl.crypto.implicits._
import co.topl.utils.IdiomaticScalaTransition.implicits.toEitherOps
import co.topl.utils.StringDataTypes.Base58Data
import co.topl.utils.StringDataTypes.implicits.showBase58String
import co.topl.utils.codecs._
import co.topl.utils.codecs.binary.legacy.BifrostSerializer
import co.topl.utils.codecs.binary.legacy.attestation.EvidenceSerializer
import co.topl.utils.codecs.binary.typeclasses.Transmittable
import com.google.common.primitives.Ints
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}
import io.estatico.newtype.macros.newtype

import scala.language.implicitConversions

/**
 * Evidence content serves as a fingerprint (or commitment) of a particular proposition that is used to lock a box. Boxes
 * are locked with 'Evidence' which is the concatentation of a typePrefix ++ content. The type prefix denotes what type
 * of proposition the content references and the content serves as the commitment that a proposition will be checked
 * against when a box is being unlocked during a transaction.
 *
 * @param evBytes an array of bytes of length 'contentLength' (currently 32 bytes) generated from a proposition
 */
final case class Evidence(evBytes: Array[Byte]) {

  def serializer: BifrostSerializer[Evidence] = EvidenceSerializer

  override def toString: String = Transmittable[Evidence].transmittableBase58(this).show

  override def equals(obj: Any): Boolean = obj match {
    case ec: Evidence => Transmittable[Evidence].transmittableBytes(this) sameElements ec.transmittableBytes
    case _            => false
  }

  override def hashCode(): Int = Ints.fromByteArray(Transmittable[Evidence].transmittableBytes(this))
}

object Evidence {
  // below are types and values used enforce the behavior of evidence
  type EvidenceTypePrefix = Byte

  @newtype
  case class EvidenceContent(value: Array[Byte])

  object EvidenceContent {
    def apply[D: Digest](d: D): EvidenceContent = EvidenceContent(d.bytes)
  }

  val contentLength = 32 //bytes (this is generally the output of a Blake2b-256 bit hash)
  val size: Int = 1 + contentLength //length of typePrefix + contentLength

  def apply(typePrefix: EvidenceTypePrefix, content: EvidenceContent): Evidence = {
    require(content.value.length == contentLength, "Invalid evidence: incorrect EvidenceContent length")

    Evidence(typePrefix +: content.value)
  }

  // see circe documentation for custom encoder / decoders
  // https://circe.github.io/circe/codecs/custom-codecs.html
  implicit val jsonEncoder: Encoder[Evidence] = _.transmittableBase58.asJson
  implicit val jsonKeyEncoder: KeyEncoder[Evidence] = _.transmittableBase58.show

  implicit val jsonDecoder: Decoder[Evidence] =
    Decoder[Base58Data].emap(_.value.decodeTransmitted[Evidence])

  implicit val jsonKeyDecoder: KeyDecoder[Evidence] =
    KeyDecoder[Base58Data].map(_.value.decodeTransmitted[Evidence].getOrThrow())
}
