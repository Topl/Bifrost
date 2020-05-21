package bifrost.transaction

import bifrost.modifier.transaction.bifrostTransaction.ArbitTransfer
import bifrost.state.BifrostState
import bifrost.{BifrostGenerators, ValidGenerators}
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

class ArbitTransferSpec extends PropSpec
  with ScalaCheckPropertyChecks
  with ScalaCheckDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators {

  property("Randomly generated ArbitTransfer Tx should be valid") {
    forAll(validArbitTransferGen) {
      at: ArbitTransfer => BifrostState.semanticValidity(at).isSuccess shouldBe true
    }
  }
}
