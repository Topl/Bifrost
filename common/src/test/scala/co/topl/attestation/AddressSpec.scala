package co.topl.attestation

import cats.scalatest.{ValidatedMatchers, ValidatedNecMatchers}
import co.topl.attestation.AddressCodec.implicits._
import co.topl.codecs.binary._
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.StringDataTypes.Base58Data
import co.topl.utils.{CommonGenerators, NetworkType}
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class AddressSpec
    extends AnyPropSpec
    with ScalaCheckDrivenPropertyChecks
    with Matchers
    with CommonGenerators
    with EitherValues
    with ValidatedMatchers
    with ValidatedNecMatchers {

  property("Applying address string with incorrect networkPrefix will result in error") {
    forAll(propositionGen) { pubkey: Proposition =>
      val twoNetworkType = scala.util.Random.shuffle(NetworkType.all).take(2)
      val fstNetworkType: NetworkType = twoNetworkType.head
      val secNetworkType: NetworkType = twoNetworkType.last

      val base58Data = {
        implicit val networkPrefix: NetworkPrefix = fstNetworkType.netPrefix
        val address: Address = pubkey.address
        address.encodeAsBase58
      }
      {
        implicit val networkPrefix: NetworkPrefix = secNetworkType.netPrefix
        base58Data.decodeAddress should haveInvalidC[AddressValidationError](NetworkTypeMismatch)
      }
    }
  }

  property("Applying address with incorrect content that doesn't match the checksum will result in error") {
    forAll(propositionGen) { pubkey: Proposition =>
      implicit val networkPrefix: NetworkPrefix = NetworkType.Mainnet.netPrefix
      val address: Address = pubkey.address
      val addressBytes: Array[Byte] = address.encodeAsBytes

      /** Alter the third byte in the address, which should be the first byte of the content */
      val corruptByte: Byte = (addressBytes(2).toInt + 1).toByte
      val modedAddrByte: Array[Byte] =
        addressBytes.slice(0, 2) ++ Array(corruptByte) ++ addressBytes.slice(3, addressBytes.length)
      val modedAddrBase58: Base58Data = modedAddrByte.encodeAsBase58

      addressBytes should not contain theSameElementsInOrderAs(modedAddrByte)

      modedAddrBase58.decodeAddress should haveInvalidC[AddressValidationError](InvalidChecksum)
    }
  }

  property("Applying address with incorrect checksum will result in error") {
    forAll(propositionGen) { pubkey: Proposition =>
      implicit val networkPrefix: NetworkPrefix = NetworkType.Mainnet.netPrefix
      val address: Address = pubkey.address
      val addrByte: Array[Byte] = address.encodeAsBytes

      /** Alter the last byte in the address, which is part of the checksum */
      val corruptByte: Byte = (addrByte(addrByte.length - 1).toInt + 1).toByte
      val modedAddrByte: Array[Byte] = addrByte.slice(0, addrByte.length - 1) ++ Array(corruptByte)
      val modedAddrBase58: Base58Data = modedAddrByte.encodeAsBase58

      addrByte should not contain theSameElementsInOrderAs(modedAddrByte)

      modedAddrBase58.decodeAddress should haveInvalidC[AddressValidationError](InvalidChecksum)
    }
  }

  property("Applying address with incorrect length will result in error") {
    forAll(propositionGen) { pubkey: Proposition =>
      implicit val networkPrefix: NetworkPrefix = NetworkType.Mainnet.netPrefix
      val address: Address = pubkey.address
      val addrByte: Array[Byte] = address.encodeAsBytes

      /** Alter the last byte in the address, which is part of the checksum */
      val corruptByte: Byte = (addrByte(addrByte.length - 1).toInt + 1).toByte
      val modedAddrByte: Array[Byte] = addrByte.slice(0, addrByte.length) ++ Array(corruptByte)
      val modedAddrBase58: Base58Data = modedAddrByte.encodeAsBase58

      addrByte should not contain theSameElementsInOrderAs(modedAddrByte)

      modedAddrBase58.decodeAddress should haveInvalidC[AddressValidationError](InvalidAddressLength)
    }
  }

  property("Applying address with incorrect NetworkPrefix will result in error") {
    forAll(propositionGen) { pubkey: Proposition =>
      implicit val networkPrefix: NetworkPrefix = -42: Byte
      val address: Address = pubkey.address
      val addrBase58: Base58Data = address.encodeAsBase58

      addrBase58.decodeAddress should haveInvalidC[AddressValidationError](InvalidNetworkPrefix)
    }
  }
}
