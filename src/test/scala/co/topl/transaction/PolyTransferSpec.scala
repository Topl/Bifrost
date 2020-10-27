package co.topl.transaction

import co.topl.modifier.transaction.PolyTransfer
import co.topl.nodeView.state.State
import co.topl.{CoreGenerators, ValidGenerators}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks }

class PolyTransferSpec extends AnyPropSpec
  with ScalaCheckPropertyChecks
  with ScalaCheckDrivenPropertyChecks
  with Matchers
  with CoreGenerators
  with ValidGenerators {

  property("Generated PolyTransfer Tx should be valid") {
    forAll(validPolyTransferGen) {
      polyTransfer: PolyTransfer =>
        State.syntacticValidity(polyTransfer).isSuccess shouldBe true
    }
  }

  property("Attempting to validate a PolyTransfer without valid signature should error") {
    // Create invalid PolyTransfer
    // send tx to state
    forAll(polyTransferGen) { polyTransfer =>
      State.syntacticValidity(polyTransfer).isSuccess shouldBe false
    }
  }

  property("Attempting to validate a ArbitTransfer without valid signature should error") {
    // Create invalid PolyTransfer
    // send tx to state
    forAll(arbitTransferGen) { arbitTransfer =>
      State.syntacticValidity(arbitTransfer).isSuccess shouldBe false
    }
  }
}
