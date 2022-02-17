package co.topl.attestation

import co.topl.attestation.EvidenceProducer.Syntax._
import co.topl.codecs.binary.legacy.attestation.AddressSerializer
import co.topl.codecs.binary.legacy.{BifrostSerializer, BytesSerializable}
import co.topl.crypto.hash.blake2b256
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.encode.Base58
import com.google.common.primitives.Ints

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
case class Address(evidence: Evidence)(implicit val networkPrefix: NetworkPrefix) extends BytesSerializable {

  @deprecated
  type M = Address

  override def toString: String = Base58.encode(bytes ++ Address.checksum(bytes))

  @deprecated
  override def serializer: BifrostSerializer[Address] = AddressSerializer

  override def equals(obj: Any): Boolean = obj match {
    case addr: Address => bytes sameElements addr.bytes
    case _             => false
  }

  override def hashCode(): Int = Ints.fromByteArray(bytes)
}

object Address {
  // the byte length of an address (network prefix + Evidence type + evidence content)
  val addressSize: Int = 1 + Evidence.size

  /**
   * The length of an address's checksum
   */
  val ChecksumLength = 4

  /**
   * Generates an Address from a proppsition. This method enables propositions to have an accessor method
   * like .address that will return the Address for that instance of the proposition.
   */
  def from[P <: Proposition: EvidenceProducer](proposition: P)(implicit networkPrefix: NetworkPrefix): Address =
    Address(proposition.generateEvidence)

  def checksum(bytes: Array[Byte]): Array[Byte] = blake2b256.hash(bytes).value.take(ChecksumLength)
}
