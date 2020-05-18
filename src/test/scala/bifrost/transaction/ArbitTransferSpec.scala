package bifrost.transaction

import bifrost.{BifrostGenerators, ValidGenerators}
import bifrost.state.State
import bifrost.modifier.transaction.bifrostTransaction.ArbitTransfer
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}

class ArbitTransferSpec extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators {

  property("Randomly generated ArbitTransfer Tx should be valid") {
    forAll(validArbitTransferGen) {
      at: ArbitTransfer => State.semanticValidity(at).isSuccess shouldBe true
    }
  }
}
