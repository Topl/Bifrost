package co.topl.attestation.address

import co.topl.attestation.Evidence
import scorex.crypto.hash.Blake2b256
import scorex.util.encode.Base58

import scala.util.Try

object AddressEncoder {
  type NetworkPrefix = Byte

  val checksumLength = 4

  //addresses are 38 bytes (1 for network prefix, 1 for type prefix, 32 for content, 4 for checksum)
  val encodeEvidenceLength: Int = 2 + Evidence.contentLength + checksumLength

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
    val addressBytes = addr.bytes
    val checksum = genChecksum(addressBytes)

    Base58.encode(addressBytes ++ checksum)
  }

  def fromString(addrStr: String): Try[Address] = {
    Base58.decode(addrStr).flatMap { bytes =>
      require(bytes.length == encodeEvidenceLength, s"Invalid address: Not the required length")
      require(genChecksum(bytes.dropRight(checksumLength)) sameElements bytes.takeRight(checksumLength),
        s"Invalid address: Checksum fails for $addrStr")

      AddressSerializer.parseBytes(bytes)
    }
  }
}
