package co.topl.attestation.evidence

import co.topl.attestation.evidence.Evidence.{ AddressContent, AddressTypePrefix }
import co.topl.attestation.proposition.KnowledgeProposition
import co.topl.attestation.secrets.{ PrivateKey25519, Secret }
import co.topl.attestation.serialization.ToplAddressSerializer
import co.topl.utils.serialization.BifrostSerializer
import com.google.common.primitives.Ints
import io.circe.syntax.EncoderOps
import io.circe.{ Decoder, Encoder, KeyDecoder, KeyEncoder }
import supertagged.TaggedType

import scala.util.{ Failure, Success }

trait Evidence extends KnowledgeProposition[PrivateKey25519] {

  type M = Evidence

  implicit val addressEncoder: AddressEncoder

  private[attestation] val addressTypePrefix: AddressTypePrefix
  private[attestation] val content: AddressContent

  override def toString: String = addressEncoder.toString(this)

  override def serializer: BifrostSerializer[Evidence] = ToplAddressSerializer

  override def equals(obj: Any): Boolean = obj match {
    case addr: Evidence => bytes sameElements addr.bytes
    case _              => false
  }

  override def hashCode(): Int = Ints.fromByteArray(bytes)
}

object Evidence {
  type AddressTypePrefix = Byte

  object AddressContent extends TaggedType[Array[Byte]]
  type AddressContent = AddressContent.Type

  val addressContentLength = 32 //bytes

  def fromString(addrStr: String)(implicit addressEncoder: AddressEncoder): Evidence[_ <: Secret] =
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

