package co.topl.attestation

import co.topl.utils.AsBytes.implicits._
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.encode.Base58
import co.topl.utils.{CoreGenerators, NetworkType, ValidGenerators}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

class AddressSpec
    extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with Matchers
    with CoreGenerators
    with ValidGenerators {

  property("Applying address string with incorrect networkPrefix will result in error") {
    forAll(propositionGen) { pubkey: PublicKeyPropositionCurve25519 =>
      val twoNetworkType = scala.util.Random.shuffle(NetworkType.all).take(2)
      val fstNetworkType: NetworkType = twoNetworkType.head
      val secNetworkType: NetworkType = twoNetworkType.last

      implicit var networkPrefix: NetworkPrefix = fstNetworkType.netPrefix
      val address: Address = pubkey.address
      val addrStr = address.toString

      networkPrefix = secNetworkType.netPrefix
      val thrown = intercept[Exception](Address(networkPrefix)(addrStr))

      thrown.getMessage shouldEqual "NetworkTypeMismatch"
    }
  }

  property("Applying address with incorrect content that doesn't match the checksum will result in error") {
    forAll(propositionGen) { pubkey: PublicKeyPropositionCurve25519 =>
      implicit val networkPrefix: NetworkPrefix = NetworkType.Mainnet.netPrefix
      val address: Address = pubkey.address
      val addrStr: String = address.toString
      val addrByte: Array[Byte] = Base58.decode(addrStr).get

      /** Alter the third byte in the address, which should be the first byte of the content */
      val corruptByte: Byte = (addrByte(2).toInt + 1).toByte
      val modedAddrByte: Array[Byte] = addrByte.slice(0, 2) ++ Array(corruptByte) ++ addrByte.slice(3, addrByte.length)
      val modedAddrStr: String = Base58.encode(modedAddrByte)

      assert(!(addrByte sameElements modedAddrByte))

      val thrown = intercept[Exception](Address(networkPrefix)(modedAddrStr))

      thrown.getMessage shouldEqual s"requirement failed: Invalid address: Checksum fails for $modedAddrStr"
    }
  }

  property("Applying address with incorrect checksum will result in error") {
    forAll(propositionGen) { pubkey: PublicKeyPropositionCurve25519 =>
      implicit val networkPrefix: NetworkPrefix = NetworkType.Mainnet.netPrefix
      val address: Address = pubkey.address
      val addrStr: String = address.toString
      val addrByte: Array[Byte] = Base58.decode(addrStr).get

      /** Alter the last byte in the address, which is part of the checksum */
      val corruptByte: Byte = (addrByte(addrByte.length - 1).toInt + 1).toByte
      val modedAddrByte: Array[Byte] = addrByte.slice(0, addrByte.length - 1) ++ Array(corruptByte)
      val modedAddrStr: String = Base58.encode(modedAddrByte)

      assert(!(addrByte sameElements modedAddrByte))

      val thrown = intercept[Exception] {
        Address(networkPrefix)(modedAddrStr)
      }
      thrown.getMessage shouldEqual s"requirement failed: Invalid address: Checksum fails for $modedAddrStr"
    }
  }

  property("Applying address with incorrect length will result in error") {
    forAll(propositionGen) { pubkey: PublicKeyPropositionCurve25519 =>
      implicit val networkPrefix: NetworkPrefix = NetworkType.Mainnet.netPrefix
      val address: Address = pubkey.address
      val addrStr: String = address.toString
      val addrByte: Array[Byte] = Base58.decode(addrStr).get

      /** Alter the last byte in the address, which is part of the checksum */
      val corruptByte: Byte = (addrByte(addrByte.length - 1).toInt + 1).toByte
      val modedAddrByte: Array[Byte] = addrByte.slice(0, addrByte.length) ++ Array(corruptByte)
      val modedAddrStr: String = Base58.encode(modedAddrByte)

      assert(!(addrByte sameElements modedAddrByte))

      val thrown = intercept[Exception] {
        Address(networkPrefix)(modedAddrStr)
      }
      thrown.getMessage shouldEqual s"requirement failed: Invalid address: Not the required length"
    }
  }

  property("Applying address with incorrect NetworkPrefix will result in error") {
    forAll(propositionGen) { pubkey: PublicKeyPropositionCurve25519 =>
      implicit val networkPrefix: NetworkPrefix = -42: Byte
      val address: Address = pubkey.address
      val addrStr: String = address.toString

      val thrown = intercept[Exception](Address(networkPrefix)(addrStr))
      thrown.getMessage shouldEqual "InvalidNetworkPrefix"
    }
  }
}
