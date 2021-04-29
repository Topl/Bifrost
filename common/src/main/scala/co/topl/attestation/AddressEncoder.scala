package co.topl.attestation

import cats.implicits._
import co.topl.utils.NetworkType
import co.topl.crypto.utils.Base58
import co.topl.crypto.hash.{Blake2b256, Digest32, Hash}
import co.topl.crypto.Implicits._
import co.topl.crypto.BytesOf
import co.topl.utils.NetworkType.NetworkPrefix

import scala.util.Try

/**
 * The Address encoder dictates how addresses are cast To and From strings. Since this is the primary
 * method users will interact with the protocol, the Address encoder adds a 4 byte checksum to the Address
 * as a quick check that may be used with external systems.
 */
object AddressEncoder {

  val checksumLength = 4

  //encoded addresses are 38 bytes (1 for network prefix, 1 for type prefix, 32 for content, 4 for checksum)
  // ENCODED ADDRESS != ADDRESS (Address are contained in an encoded address)
  private val encodedAddressLength: Int = Address.addressSize + checksumLength

  /**
   * Generates a checksum value for checking correctness of string parsed addresses
   *
   * @param addrBytes the bytes of an address (1 - networkPrefix, 1 - addressTypePres, 32 - content bytes)
   * @return a 4 byte checksum value
   */
  private def genChecksum(addrBytes: Array[Byte]): Array[Byte] =
    BytesOf[Array[Byte]].take(Hash[Blake2b256, Digest32].hash(addrBytes), checksumLength)

  def toString(addr: Address): String = {
    val addrBytes = addr.bytes
    val checksum = genChecksum(addrBytes)

    Base58.encode(addrBytes ++ checksum)
  }

  /**
   * Parse an Address from a string (without checking that the network matches)
   * @param addrStr
   * @return
   */
  def fromStringUnsafe(addrStr: String): Try[Address] = Base58.decode(addrStr).flatMap(fromBytes)

  /**
   * Parse an Address from a string ensuring that the networkPrefix is correct
   * @param addrStr a Base58 encoded address
   * @param networkPrefix a single byte used to identify a network
   * @return the network prefix of the address
   */
  def fromStringWithCheck(
    addrStr:       String,
    networkPrefix: NetworkType.NetworkPrefix
  ): Either[AddressValidationError, Address] =
    Base58.decode(addrStr).toEither.leftMap(_ => InvalidAddress).flatMap(validateAddress(_, networkPrefix))

  def validateAddress(decoded: Array[Byte], networkPrefix: NetworkPrefix): Either[AddressValidationError, Address] =
    for {
      decodedPrefix <- decoded.headOption.toRight(InvalidAddress)
      // A user can pass in an invalid networkPrefix, so we must validate that it is real
      nt <- NetworkType.pickNetworkType(networkPrefix).toRight(InvalidNetworkPrefix)
      address <-
        if (nt.netPrefix == decodedPrefix) fromBytes(decoded).toEither.leftMap(_ => InvalidAddress)
        else Left(NetworkTypeMismatch)
    } yield address

  private def fromBytes(bytes: Array[Byte]): Try[Address] = {
    require(bytes.length == encodedAddressLength, s"Invalid address: Not the required length")

    val addrBytes = bytes.dropRight(checksumLength)
    val checksum = bytes.takeRight(checksumLength)
    require(
      genChecksum(addrBytes) sameElements checksum,
      s"Invalid address: Checksum fails for ${Base58.encode(bytes)}"
    )

    Address.parseBytes(addrBytes)
  }
}

sealed abstract class AddressValidationError
case object InvalidNetworkPrefix extends AddressValidationError
case object InvalidAddress extends AddressValidationError
case object NetworkTypeMismatch extends AddressValidationError
