package co.topl.transaction

import co.topl.modifier.transaction.ArbitTransfer
import co.topl.nodeView.state.State
import co.topl.{ BifrostGenerators, ValidGenerators }
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks }

class ArbitTransferSpec extends AnyPropSpec
  with ScalaCheckPropertyChecks
  with ScalaCheckDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators {

  property("Randomly generated ArbitTransfer Tx should be valid") {
    forAll(validArbitTransferGen) {
      at: ArbitTransfer => State.syntacticValidity(at).isSuccess shouldBe true
    }
  }
}
