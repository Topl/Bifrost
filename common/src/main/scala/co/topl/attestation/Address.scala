package co.topl.attestation

import cats.implicits._
import cats.{Eq, Show}
import co.topl.attestation.EvidenceProducer.Syntax._
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.StringDataTypes.Base58Data
import co.topl.utils.catsInstances._
import co.topl.utils.codecs._
import co.topl.utils.codecs.binary.legacy.BifrostSerializer
import co.topl.utils.codecs.binary.legacy.attestation.AddressSerializer
import co.topl.utils.codecs.binary.typeclasses.Transmittable
import com.google.common.primitives.Ints
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}
import co.topl.attestation.AddressCodec.implicits._

/**
 * An address is a network specific commitment to a proposition encumbering a box. Addresses incorporate the evidence type
 * and content from a proposition and add a network specific prefix.
 * NOTE: when converted to/from string encodings Addresses will include a 4 byte checksum to allow for external
 * software to enforce correctness checks (this behavior is controlled in the AddressEncoder). This 4 byte checksum is
 * not included in the byte serialization of an Address.
 *
 * @param evidence a commitment produced from a proposition that identifies that proposition.
 * @param networkPrefix a runtime specified parameter denoting the type of network that is executing
 */
case class Address(evidence: Evidence)(implicit val networkPrefix: NetworkPrefix) {

  override def toString: String = Show[Address].show(this)

  def serializer: BifrostSerializer[Address] = AddressSerializer

  override def equals(obj: Any): Boolean = obj match {
    case addr: Address => Eq[Address].eqv(this, addr)
    case _             => false
  }

  override def hashCode(): Int = Ints.fromByteArray(Transmittable[Address].transmittableBytes(this))
}

object Address {
  // the byte length of an address (network prefix + Evidence type + evidence content)
  val addressSize: Int = 1 + Evidence.size

  implicit val jsonEncoder: Encoder[Address] = (addr: Address) => addr.toString.asJson
  implicit val jsonKeyEncoder: KeyEncoder[Address] = (addr: Address) => addr.toString

  implicit def jsonDecoder(implicit networkPrefix: NetworkPrefix): Decoder[Address] =
    Decoder[Base58Data].emap(x => x.decodeAddress.toEither.leftMap(_.toString))

  implicit def jsonKeyDecoder(implicit networkPrefix: NetworkPrefix): KeyDecoder[Address] =
    json => Base58Data.validated(json).toOption.flatMap(_.decodeAddress.toOption)

  /**
   * Generates an Address from a proppsition. This method enables propositions to have an accessor method
   * like .address that will return the Address for that instance of the proposition.
   */
  def from[P <: Proposition: EvidenceProducer](proposition: P)(implicit networkPrefix: NetworkPrefix): Address =
    Address(proposition.generateEvidence)
}
