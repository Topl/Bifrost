package co.topl.modifier.transaction

import co.topl.utils.NetworkType.PrivateTestnet
import co.topl.utils.{KeyFileTestHelper, NetworkType, NodeGenerators}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.forAll

class CrossBroadcastSpec extends AnyPropSpec with KeyFileTestHelper with NodeGenerators with BeforeAndAfterAll {

  property("Transactions created on a specific network should not be accepted on any other network") {
    forAll(validAssetTransfer(keyRing, genesisState, minting = true)) { tx =>
      val otherNetworks = NetworkType.all.filterNot(_ == PrivateTestnet)
      otherNetworks.foreach { netType =>
        tx.syntacticValidate(netType.netPrefix).isInvalid shouldBe true
      }
      tx.syntacticValidate(PrivateTestnet.netPrefix).isValid shouldBe true
    }
  }
}
