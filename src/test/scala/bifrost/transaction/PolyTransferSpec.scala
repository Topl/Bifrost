package bifrost.transaction

import bifrost.{BifrostGenerators, ValidGenerators}
import bifrost.state.BifrostState
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}

class PolyTransferSpec extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators {


  property("Attempting to validate a PolyTransfer without valid signature should error") {
    // Create invalid PolyTransfer
    // send tx to state
    forAll(polyTransferGen) { tx =>
      BifrostState.semanticValidity(tx).isSuccess shouldBe false
    }
  }


  property("Attempting to validate a ArbitTransfer without valid signature should error") {
    // Create invalid PolyTransfer
    // send tx to state
    forAll(arbitTransferGen) { tx =>
      BifrostState.semanticValidity(tx).isSuccess shouldBe false
    }
  }
}
