package co.topl.attestation

import cats.scalatest.{ValidatedMatchers, ValidatedNecMatchers}
import co.topl.attestation.AddressCodec.implicits.StringOps
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.codecs.AsBytes.implicits._
import co.topl.utils.encode.Base58
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

      val addrStr = {
        implicit val networkPrefix: NetworkPrefix = fstNetworkType.netPrefix
        val address: Address = pubkey.address
        address.toString
      }
      {
        implicit val networkPrefix: NetworkPrefix = secNetworkType.netPrefix
        addrStr.decodeAddress should haveInvalidC[AddressValidationError](NetworkTypeMismatch)
      }
    }
  }

  property("Applying address with incorrect content that doesn't match the checksum will result in error") {
    forAll(propositionGen) { pubkey: Proposition =>
      implicit val networkPrefix: NetworkPrefix = NetworkType.Mainnet.netPrefix
      val address: Address = pubkey.address
      val addrStr: String = address.toString
      val addrByte: Array[Byte] = Base58.decode(addrStr).get

      /** Alter the third byte in the address, which should be the first byte of the content */
      val corruptByte: Byte = (addrByte(2).toInt + 1).toByte
      val modedAddrByte: Array[Byte] = addrByte.slice(0, 2) ++ Array(corruptByte) ++ addrByte.slice(3, addrByte.length)
      val modedAddrStr: String = Base58.encode(modedAddrByte)

      addrByte should not contain theSameElementsInOrderAs(modedAddrByte)

      modedAddrStr.decodeAddress should haveInvalidC[AddressValidationError](InvalidChecksum)
    }
  }

  property("Applying address with incorrect checksum will result in error") {
    forAll(propositionGen) { pubkey: Proposition =>
      implicit val networkPrefix: NetworkPrefix = NetworkType.Mainnet.netPrefix
      val address: Address = pubkey.address
      val addrStr: String = address.toString
      val addrByte: Array[Byte] = Base58.decode(addrStr).get

      /** Alter the last byte in the address, which is part of the checksum */
      val corruptByte: Byte = (addrByte(addrByte.length - 1).toInt + 1).toByte
      val modedAddrByte: Array[Byte] = addrByte.slice(0, addrByte.length - 1) ++ Array(corruptByte)
      val modedAddrStr: String = Base58.encode(modedAddrByte)

      addrByte should not contain theSameElementsInOrderAs(modedAddrByte)

      modedAddrStr.decodeAddress should haveInvalidC[AddressValidationError](InvalidChecksum)
    }
  }

  property("Applying non-base58 encoded address will result in error") {
    implicit val networkPrefix: NetworkPrefix = NetworkType.Mainnet.netPrefix
    val addressStr: String = "0OIlL+/"

    addressStr.decodeAddress should haveInvalidC[AddressValidationError](NotBase58)
  }

  property("Applying address with incorrect length will result in error") {
    forAll(propositionGen) { pubkey: Proposition =>
      implicit val networkPrefix: NetworkPrefix = NetworkType.Mainnet.netPrefix
      val address: Address = pubkey.address
      val addrStr: String = address.toString
      val addrByte: Array[Byte] = Base58.decode(addrStr).get

      /** Alter the last byte in the address, which is part of the checksum */
      val corruptByte: Byte = (addrByte(addrByte.length - 1).toInt + 1).toByte
      val modedAddrByte: Array[Byte] = addrByte.slice(0, addrByte.length) ++ Array(corruptByte)
      val modedAddrStr: String = Base58.encode(modedAddrByte)

      addrByte should not contain theSameElementsInOrderAs(modedAddrByte)

      modedAddrStr.decodeAddress should haveInvalidC[AddressValidationError](InvalidAddressLength)
    }
  }

  property("Applying address with incorrect NetworkPrefix will result in error") {
    forAll(propositionGen) { pubkey: Proposition =>
      implicit val networkPrefix: NetworkPrefix = -42: Byte
      val address: Address = pubkey.address
      val addrStr: String = address.toString

      addrStr.decodeAddress should haveInvalidC[AddressValidationError](InvalidNetworkPrefix)
    }
  }
}
