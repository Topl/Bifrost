package bifrost.transaction

import bifrost.{BifrostGenerators, ValidGenerators}
import bifrost.state.State
import bifrost.modifier.transaction.bifrostTransaction.ArbitTransfer
import org.scalatestplus.scalacheck.{ ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks }
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

class ArbitTransferSpec extends AnyPropSpec
  with ScalaCheckPropertyChecks
  with ScalaCheckDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators {

  property("Randomly generated ArbitTransfer Tx should be valid") {
    forAll(validArbitTransferGen) {
      at: ArbitTransfer => State.semanticValidity(at).isSuccess shouldBe true
    }
  }
}
