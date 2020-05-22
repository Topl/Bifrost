package bifrost.transaction

import bifrost.{BifrostGenerators, ValidGenerators}
import bifrost.state.State
import bifrost.modifier.transaction.bifrostTransaction.ArbitTransfer
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks, ScalaCheckDrivenPropertyChecks}

class ArbitTransferSpec extends PropSpec
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
