package crypto

import scorex.crypto.hash.Blake2b256

import scala.util.Try
import scorex.util.encode.Base58

/**
  * The Address encoder dictates how addresses are cast To and From strings. Since this is the primary
  * method users will interact with the protocol, the Address encoder adds a 4 byte checksum to the Address
  * as a quick check that may be used with external systems.
  */
object AddressEncoder {
  type NetworkPrefix = Byte

  val checksumLength = 4

  //encoded addresses are 38 bytes (1 for network prefix, 1 for type prefix, 32 for content, 4 for checksum)
  // ENCODED ADDRESS != ADDRESS (Address are contained in an encoded address)
  private val encodedAddressLength: Int = Address.addressSize + checksumLength

  /**
    * Helper method to query the network prefix in an encoded address string
    * @param addrStr a Base58 encoded address
    * @return the network prefix of the address
    */
  def matchesNetworkPrefix(addrStr: String)(implicit networkPrefix: NetworkPrefix): Boolean =
    Base58.decode(addrStr).fold[Boolean](_ => false, _.head == networkPrefix)

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

  def fromString(addrStr: String): Try[Address] = {
    Base58.decode(addrStr).flatMap { bytes =>
      require(bytes.length == encodedAddressLength, s"Invalid address: the length is ${bytes.length}, not the required length of $encodedAddressLength")

      val addrBytes = bytes.dropRight(checksumLength)
      val checksum = bytes.takeRight(checksumLength)
      require(genChecksum(addrBytes) sameElements checksum, s"Invalid address: Checksum fails for $addrStr")

      Address.parseBytes(addrBytes)
    }
  }
}
