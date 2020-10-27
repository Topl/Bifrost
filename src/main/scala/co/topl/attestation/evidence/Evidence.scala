package co.topl.attestation.evidence

import co.topl.attestation.evidence.Evidence.{ EvidenceContent, EvidenceTypePrefix }
import co.topl.attestation.proposition.Proposition
import co.topl.attestation.serialization.ToplAddressSerializer
import co.topl.utils.serialization.{ BifrostSerializer, BytesSerializable }
import com.google.common.primitives.Ints
import io.circe.syntax.EncoderOps
import io.circe.{ Decoder, Encoder, KeyDecoder, KeyEncoder }
import supertagged.TaggedType

import scala.util.{ Failure, Success }

trait Evidence[P <: Proposition] extends BytesSerializable {

  type M = Evidence[P]

  implicit val evidenceEncoder: EvidenceEncoder

  private[attestation] val networkPrefix: evidenceEncoder.networkPrefix
  private[attestation] val typePrefix: EvidenceTypePrefix
  private[attestation] val content: EvidenceContent

  override def toString: String = evidenceEncoder.toString(this)

  override def serializer: BifrostSerializer[Evidence[P]] = ToplAddressSerializer

  override def equals(obj: Any): Boolean = obj match {
    case addr: Evidence[_] => bytes sameElements addr.bytes
    case _                 => false
  }

  override def hashCode(): Int = Ints.fromByteArray(bytes)
}

object Evidence {
  type EvidenceTypePrefix = Byte

  object EvidenceContent extends TaggedType[Array[Byte]]
  type EvidenceContent = EvidenceContent.Type

  val contentLength = 32 //bytes

  def fromString(addrStr: String)(implicit addressEncoder: EvidenceEncoder): Evidence =
    addressEncoder.fromString(addrStr) match {
      case Success(addr) => addr
      case Failure(ex)   => throw new Error(s"Invalid address: Failed to parse address with exception $ex")
    }

  implicit val jsonEncoder: Encoder[Evidence] =
    (prop: Evidence) => prop.toString.asJson

  implicit val jsonDecoder: Decoder[Evidence] =
    Decoder.decodeString.emapTry(Try(fromString(_)))

  implicit val jsonKeyEncoder: KeyEncoder[Evidence] =
    (prop: Evidence) => prop.toString

  implicit val jsonKeyDecoder: KeyDecoder[Evidence] =
    (prop: String) => PublicKey25519Proposition.validAddress(prop).toOption
}

