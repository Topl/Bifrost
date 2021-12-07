package co.topl.attestation

import cats.implicits.toShow
import co.topl.crypto.hash.digest.Digest
import co.topl.crypto.implicits._
import co.topl.utils.IdiomaticScalaTransition.implicits.toEitherOps
import co.topl.utils.StringDataTypes.Base58Data
import co.topl.utils.StringDataTypes.implicits.showBase58String
import co.topl.codecs._
import co.topl.codecs.binary.legacy.attestation.EvidenceSerializer
import co.topl.codecs.binary.legacy.{BifrostSerializer, BytesSerializable}
import co.topl.utils.encode.Base58
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
final case class Evidence(evBytes: Array[Byte]) extends BytesSerializable {

  @deprecated
  type M = Evidence

  @deprecated
  override def serializer: BifrostSerializer[Evidence] = EvidenceSerializer

  @deprecated
  override def toString: String = Base58.encode(bytes)

  @deprecated
  override def equals(obj: Any): Boolean = obj match {
    case ec: Evidence => bytes sameElements ec.bytes
    case _            => false
  }

  @deprecated
  override def hashCode(): Int = Ints.fromByteArray(bytes)
}

object Evidence {
  // below are types and values used enforce the behavior of evidence
  type EvidenceTypePrefix = Byte

  @newtype
  case class EvidenceContent(value: Array[Byte])

  object EvidenceContent {
    def apply[D: Digest](d: D): EvidenceContent = EvidenceContent(d.bytes)
  }

  val contentLength = 32 // bytes (this is generally the output of a Blake2b-256 bit hash)
  val size: Int = 1 + contentLength // length of typePrefix + contentLength

  def apply(typePrefix: EvidenceTypePrefix, content: EvidenceContent): Evidence = {
    require(content.value.length == contentLength, "Invalid evidence: incorrect EvidenceContent length")

    Evidence(typePrefix +: content.value)
  }
}
