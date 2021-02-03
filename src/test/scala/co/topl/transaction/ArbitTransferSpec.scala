package co.topl.transaction

import co.topl.modifier.transaction.ArbitTransfer
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
    forAll(validArbitTransferGen) { arbitTransfer: ArbitTransfer[_] =>
      //TODO: Jing - change this back to using syntacticValidate once attestation in validArbitTransferGen works
      arbitTransfer.rawValidate.isSuccess shouldBe true
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
