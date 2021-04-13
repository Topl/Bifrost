package co.topl.modifier.transaction

import co.topl.utils.{CoreGenerators, ValidGenerators}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

class ArbitTransferSpec extends AnyPropSpec
  with ScalaCheckPropertyChecks
  with ScalaCheckDrivenPropertyChecks
  with Matchers
  with CoreGenerators
  with ValidGenerators {

  property("Randomly generated ArbitTransfer Tx should be valid") {
    forAll(validArbitTransfer(keyRing, genesisState)) { arbitTransfer: ArbitTransfer[_] =>
      arbitTransfer.syntacticValidate.isSuccess shouldBe true
    }
  }

  property("Attempting to validate a ArbitTransfer without valid signature should error") {
    // Create invalid PolyTransfer
    // send tx to state
    forAll(arbitTransferGen) { arbitTransfer: ArbitTransfer[_] =>
      arbitTransfer.syntacticValidate.isSuccess shouldBe false
    }
  }
}
