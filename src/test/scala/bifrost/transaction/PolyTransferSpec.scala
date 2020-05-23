package bifrost.transaction

import bifrost.{BifrostGenerators, ValidGenerators}
import bifrost.state.State
import bifrost.modifier.transaction.bifrostTransaction.PolyTransfer
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}

class PolyTransferSpec extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators {

  property("Generated PolyTransfer Tx should be valid") {
    forAll(validPolyTransferGen) {
      polyTransfer: PolyTransfer =>
        State.semanticValidity(polyTransfer).isSuccess shouldBe true
    }
  }

  property("Attempting to validate a PolyTransfer without valid signature should error") {
    // Create invalid PolyTransfer
    // send tx to state
    forAll(polyTransferGen) { polyTransfer =>
      State.semanticValidity(polyTransfer).isSuccess shouldBe false
    }
  }

  property("Attempting to validate a ArbitTransfer without valid signature should error") {
    // Create invalid PolyTransfer
    // send tx to state
    forAll(arbitTransferGen) { arbitTransfer =>
      State.semanticValidity(arbitTransfer).isSuccess shouldBe false
    }
  }
}
