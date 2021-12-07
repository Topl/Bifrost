package co.topl.attestation

import cats.implicits._
import co.topl.crypto.hash.digest.Digest
import co.topl.utils.StringDataTypes.Base58Data
import co.topl.utils.StringDataTypes.implicits._
import co.topl.utils.codecs.binary.legacy.attestation.EvidenceSerializer
import co.topl.utils.codecs.binary.legacy.{BifrostSerializer, BytesSerializable}
import co.topl.utils.codecs.json.codecs._
import co.topl.utils.codecs.binary.implicits._
import com.google.common.primitives.Ints
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}
import io.estatico.newtype.macros.newtype
import io.estatico.newtype.ops._

import scala.language.implicitConversions
import scala.util.{Failure, Success}

/**
 * Evidence content serves as a fingerprint (or commitment) of a particular proposition that is used to lock a box. Boxes
 * are locked with 'Evidence' which is the concatentation of a typePrefix ++ content. The type prefix denotes what type
 * of proposition the content references and the content serves as the commitment that a proposition will be checked
 * against when a box is being unlocked during a transaction.
 *
 * @param evBytes an array of bytes of length 'contentLength' (currently 32 bytes) generated from a proposition
 */
final case class Evidence(evBytes: Array[Byte]) extends BytesSerializable {
  override type M = Evidence
  override def serializer: BifrostSerializer[Evidence] = EvidenceSerializer

  override def toString: String = bytes.encodeAsBase58.show

  override def equals(obj: Any): Boolean = obj match {
    case ec: Evidence => bytes sameElements ec.bytes
    case _            => false
  }

  override def hashCode(): Int = Ints.fromByteArray(bytes)
}

object Evidence {
  // below are types and values used enforce the behavior of evidence
  type EvidenceTypePrefix = Byte

  @newtype
  case class EvidenceContent(value: Array[Byte])

  object EvidenceContent {
    def apply[D: Digest](d: D): EvidenceContent = d.infalliblyEncodeAsBytes.coerce
  }

  val contentLength = 32 // bytes (this is generally the output of a Blake2b-256 bit hash)
  val size: Int = 1 + contentLength // length of typePrefix + contentLength

  def apply(typePrefix: EvidenceTypePrefix, content: EvidenceContent): Evidence = {
    require(content.value.length == contentLength, "Invalid evidence: incorrect EvidenceContent length")

    EvidenceSerializer.parseBytes(typePrefix +: content.value) match {
      case Success(ec) => ec
      case Failure(ex) => throw ex
    }
  }

  private def apply(data: Base58Data): Evidence = {
    val bytes = data.value
    require(bytes.length == size, "Invalid evidence: incorrect evidence length")
    EvidenceSerializer.parseBytes(bytes) match {
      case Success(ec) => ec
      case Failure(ex) => throw ex
    }
  }

  // see circe documentation for custom encoder / decoders
  // https://circe.github.io/circe/codecs/custom-codecs.html
  implicit val jsonEncoder: Encoder[Evidence] = (ec: Evidence) => ec.toString.asJson
  implicit val jsonKeyEncoder: KeyEncoder[Evidence] = (ec: Evidence) => ec.toString
  implicit val jsonDecoder: Decoder[Evidence] = Decoder[Base58Data].map(apply)
  implicit val jsonKeyDecoder: KeyDecoder[Evidence] = KeyDecoder[Base58Data].map(apply)
}
