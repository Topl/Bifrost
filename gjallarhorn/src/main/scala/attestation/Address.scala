package attestation

import attestation.AddressEncoder.NetworkPrefix
import attestation.EvidenceProducer.Syntax._
import com.google.common.primitives.Ints
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}
import utils.serialization.{BytesSerializable, GjalSerializer, Reader, Writer}

import scala.util.{Failure, Success}

/**
 * An address is a network specific commitment to a proposition encumbering a box.
 * Addresses incorporate the evidence type and content from a proposition and add a network specific prefix.
 * NOTE: when converted to/from string encodings Addresses will include a 4 byte checksum to allow for external
 * software to enforce correctness checks (this behavior is controlled in the AddressEncoder). This 4 byte checksum is
 * not included in the byte serialization of an Address.
 *
 * @param evidence a commitment produced from a proposition that identifies that proposition.
 * @param networkPrefix a runtime specified parameter denoting the type of network that is executing
 */
case class Address(evidence: Evidence)(implicit val networkPrefix: NetworkPrefix) extends BytesSerializable {

  type M = Address

  override def toString: String = AddressEncoder.toString(this)

  override def serializer: GjalSerializer[Address] = Address

  override def equals(obj: Any): Boolean = obj match {
    case addr: Address => bytes sameElements addr.bytes
    case _             => false
  }

  override def hashCode(): Int = Ints.fromByteArray(bytes)
}

object Address extends GjalSerializer[Address] {
  // the byte length of an address (network prefix + Evidence type + evidence content)
  val addressSize: Int = 1 + Evidence.size

  implicit val jsonEncoder: Encoder[Address] = (addr: Address) => addr.toString.asJson
  implicit val jsonKeyEncoder: KeyEncoder[Address] = (addr: Address) => addr.toString

  implicit def jsonDecoder(implicit networkPrefix: NetworkPrefix): Decoder[Address] =
    Decoder.decodeString.map(str => apply(networkPrefix)(str))

  implicit def jsonKeyDecoder(implicit networkPrefix: NetworkPrefix): KeyDecoder[Address] =
    (str: String) => Some(apply(networkPrefix)(str))

  def apply(networkPrefix: NetworkPrefix)(addrStr: String): Address =
    AddressEncoder.fromStringWithCheck(addrStr, networkPrefix) match {
      case Success(addr) => addr
      case Failure(_: java.lang.AssertionError) =>
        throw new Exception(s"""The address: "$addrStr" is an invalid Base58 string""")
      case Failure(ex) => throw ex
    }

  /**
   * Generates an Address from a proposition. This method enables propositions to have an accessor method
   * like .address that will return the Address for that instance of the proposition.
   * @param proposition the proposition used to generate the address
   * @param networkPrefix a runtime specified parameter denoting the type of network that is executing
   * @tparam P the type of Proposition (PublicKeyProposition or ThresholdProposition)
   * @return returns the address for the given proposition
   */
  def from[P <: Proposition: EvidenceProducer](proposition: P)(implicit networkPrefix: NetworkPrefix): Address =
    Address(proposition.generateEvidence)

  def serialize(obj: Address, w: Writer): Unit = {
    /* networkType: Byte */
    w.put(obj.networkPrefix)

    /* addressBytes: Array[Byte] */
    Evidence.serialize(obj.evidence, w)
  }

  def parse(r: Reader): Address = {
    implicit val networkPrefix: NetworkPrefix = r.getByte()
    val evidence: Evidence = Evidence.parse(r)
    Address(evidence)
  }
}
