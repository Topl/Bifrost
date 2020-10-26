package co.topl.address

import co.topl.address.ToplAddress.{AddressContent, AddressTypePrefix}
import co.topl.address.serialization.ToplAddressSerializer
import co.topl.crypto.{PrivateKey25519, ProofOfKnowledgeProposition, Secret}
import co.topl.utils.serialization.BifrostSerializer
import com.google.common.primitives.Ints
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}
import supertagged.TaggedType

abstract class ToplAddress[S <: Secret] (private val addressTypePrefix: AddressTypePrefix, private val content: AddressContent) extends ProofOfKnowledgeProposition[S] {

  type M = ToplAddress[_ <: Secret]

  lazy val addressBytes: Array[Byte] = addressTypePrefix +: content

  def equals(obj: Any): Boolean

  override def hashCode(): Int = Ints.fromByteArray(content)

  override def serializer: BifrostSerializer[ToplAddress[_ <: Secret]] = ToplAddressSerializer

}

object ToplAddress {
  type AddressTypePrefix = Byte

  object AddressContent extends TaggedType[Array[Byte]]
  type AddressContent = AddressContent.Type

  val addressContentLength = 32 //bytes

  def matchType(addressTypePrefix: AddressTypePrefix, addressContent: AddressContent): ToplAddress[_ <: Secret] =
    addressTypePrefix match {
      case prefix @ PublicKeyAddress.addressTypePrefixCurve25519 => new PublicKeyAddress[PrivateKey25519](prefix, AddressContent @@ addressContent)
      case _ => throw new Exception("Invalid address: Unsupported address type " + addressTypePrefix)
    }

  implicit val jsonEncoder: Encoder[ToplAddress[_ <: Secret]] =
    (prop: ToplAddress[_]) => prop.toString.asJson

  implicit val jsonDecoder: Decoder[ToplAddress[_ <: Secret]] =
    Decoder.decodeString.emapTry(PublicKey25519Proposition.validAddress(_))

  implicit val jsonKeyEncoder: KeyEncoder[ToplAddress[_ <: Secret]] =
    (prop: ToplAddress[_]) => prop.toString

  implicit val jsonKeyDecoder: KeyDecoder[ToplAddress[_ <: Secret]] =
    (prop: String) => PublicKey25519Proposition.validAddress(prop).toOption
}

