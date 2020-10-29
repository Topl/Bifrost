package co.topl.attestation.address

import co.topl.attestation.Evidence
import co.topl.attestation.Evidence.syntax._
import co.topl.attestation.Evidence.{EvidenceContent, EvidenceTypePrefix}
import co.topl.attestation.address.AddressEncoder.NetworkPrefix
import co.topl.utils.serialization.{BifrostSerializer, BytesSerializable}
import com.google.common.primitives.Ints
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}

import scala.util.{Failure, Success}

case class Address( private[attestation] val typePrefix: EvidenceTypePrefix,
                    private[attestation] val content: EvidenceContent
                  ) (implicit val networkPrefix: NetworkPrefix) extends BytesSerializable {

  type M = Address

  override def toString: String = AddressEncoder.toString(this)

  override def serializer: BifrostSerializer[Address] = AddressSerializer

  override def equals(obj: Any): Boolean = obj match {
    case addr: Address => bytes sameElements addr.bytes
    case _             => false
  }

  override def hashCode(): Int = Ints.fromByteArray(bytes)
}



object Address {

  def apply(addrStr: String): Address =
    AddressEncoder.fromString(addrStr) match {
      case Success(addr) => addr
      case Failure(ex)   => throw ex
    }

  def from[E: Evidence] (proposition: E)(implicit networkPrefix: NetworkPrefix): Address =
    Address(proposition.typePrefix, proposition.generateEvidence)

  implicit val jsonEncoder: Encoder[Address] = (addr: Address) => addr.toString.asJson
  implicit val jsonKeyEncoder: KeyEncoder[Address] = (addr: Address) => addr.toString
  implicit val jsonDecoder: Decoder[Address] = Decoder.decodeString.emapTry(AddressEncoder.fromString)
  implicit val jsonKeyDecoder: KeyDecoder[Address] = AddressEncoder.fromString(_: String).toOption
}

