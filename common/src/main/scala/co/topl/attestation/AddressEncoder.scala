package co.topl.attestation

import co.topl.utils.NetworkType
import scorex.crypto.hash.Blake2b256
import scorex.util.encode.Base58

import scala.util.{Failure, Try}

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
  private def genChecksum(addrBytes: Array[Byte]): Array[Byte] = Blake2b256(addrBytes).take(checksumLength)

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
  def fromStringWithCheck(addrStr: String, networkPrefix: NetworkType.NetworkPrefix): Try[Address] =
    Base58.decode(addrStr).flatMap { b =>
      NetworkType.pickNetworkType(networkPrefix) match {
        case None => throw new Exception("Invalid networkPrefix specified")
        case Some(nt) if nt.netPrefix == b.head => fromBytes(b)
        case Some(nt) if nt.netPrefix != b.head =>
          Failure(new Exception("Invalid address: Network type does not match"))
      }
    }

  private def fromBytes(bytes: Array[Byte]): Try[Address] = {
    require(bytes.length == encodedAddressLength, s"Invalid address: Not the required length")

    val addrBytes = bytes.dropRight(checksumLength)
    val checksum = bytes.takeRight(checksumLength)
    require(genChecksum(addrBytes) sameElements checksum, s"Invalid address: Checksum fails for ${Base58.encode(bytes)}")

    Address.parseBytes(addrBytes)
  }
}
