package co.topl.address

import co.topl.address.AddressEncoder.NetworkPrefix
import co.topl.address.ToplAddress.AddressContent
import co.topl.crypto.{PrivateKey25519, Secret}
import scorex.crypto.hash.Blake2b256
import scorex.util.encode.Base58

import scala.util.Try

case class AddressEncoder(networkPrefix: NetworkPrefix) {

  import AddressEncoder._

  private def getChecksum(netAddressBytes: Array[Byte]): Array[Byte] = Blake2b256(netAddressBytes).take(checksumLength)

  def toString(address: ToplAddress[_]): String = {
    val netAddressBytes = networkPrefix +: address.addressBytes
    val checksum = getChecksum(netAddressBytes)
    Base58.encode(netAddressBytes ++ checksum)
  }

  def fromString(addressStr: String): Try[ToplAddress[_ <: Secret]] = Base58.decode(addressStr).flatMap { bytes =>
    Try {
      require(bytes.length == encodedAddressLength, s"Invalid address: Not the required length")

      val network = bytes(0)
      val addressType = bytes(1)
      val addressContent = bytes.slice(2, bytes.length - checksumLength)
      val checksum = bytes.slice(bytes.length - checksumLength, bytes.length)

      require(network == networkPrefix, s"Invalid address: Not applicable for the current network")
      require(Blake2b256(addressContent).take(checksumLength) sameElements checksum, s"Invalid address: Checksum fails for $addressStr")

      ToplAddress.matchType(addressType, AddressContent @@ addressContent)
    }
  }
}

object AddressEncoder {
  type NetworkPrefix = Byte
  val mainNetPrefix: NetworkPrefix = 1.toByte
  val testNetPrefix: NetworkPrefix = 16.toByte
  val devNetPrefix: NetworkPrefix = 32.toByte
  val localNetPrefix: NetworkPrefix = 48.toByte
  val privateNetPrefix: NetworkPrefix = 64.toByte

  val checksumLength = 4

  val encodedAddressLength: Int = 38 //addresses are 38 bytes (1 for network prefix, 1 for type prefix, 32 for content, 4 for checksum)
}
