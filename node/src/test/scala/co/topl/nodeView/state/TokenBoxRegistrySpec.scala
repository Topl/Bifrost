package co.topl.nodeView.state

import co.topl.attestation.Address
import co.topl.nodeView.NodeViewTestHelpers
import co.topl.nodeView.history.InMemoryKeyValueStore
import co.topl.utils.GeneratorOps.GeneratorOps
import co.topl.utils.{InMemoryKeyRingTestHelper, NetworkPrefixTestHelper, NodeGenerators, TestSettings}
import org.scalatest.{BeforeAndAfterAll, Suite}
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpecLike
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class TokenBoxRegistrySpec
    extends AnyPropSpecLike
    with NodeViewTestHelpers
    with InMemoryKeyRingTestHelper
    with NodeGenerators
    with NetworkPrefixTestHelper
    with ScalaCheckDrivenPropertyChecks
    with Matchers
    with BeforeAndAfterAll {

  property("Token boxes should be inserted into the registry") {
    forAll(genesisBlockGen, tokenBoxesGen) { (genesisBlock, tokens) =>
      val state = generateState(genesisBlock).state
      val keys = tokens.groupBy(_.evidence)
      directlyAddTBRStorage(modifierIdGen.sampleFirst(), tokens, state)
      keys.foreach { key =>
        val ids = key._2.map(_.id)
        state.registryLookup(Address(key._1)).value shouldEqual ids
      }
    }
  }

  property("Rolling back should remove tokens from registry") {
    forAll(genesisBlockGen, tokenBoxesGen) { (genesisBlock, tokens) =>
      val state = generateState(genesisBlock).state
      val tbr = state.tokenBoxRegistry
      val version = modifierIdGen.sampleFirst()
      val keys = tokens.groupBy(_.evidence).map(k => Address(k._1) -> k._2)
      val update = keys.map(k => k._1 -> k._2.map(_.nonce))

      val newTbr = tbr.update(version, Map(), update).get
      newTbr.rollbackTo(state.version)

      keys.foreach { key =>
        state.registryLookup(key._1) shouldBe None
      }
    }
  }
}
