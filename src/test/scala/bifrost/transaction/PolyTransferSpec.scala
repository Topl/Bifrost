package bifrost.transaction

import bifrost.{BifrostGenerators, ValidGenerators}
import bifrost.state.BifrostState
import bifrost.modifier.transaction.bifrostTransaction.PolyTransfer
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks, ScalaCheckDrivenPropertyChecks}

class PolyTransferSpec extends PropSpec
  with ScalaCheckPropertyChecks
  with ScalaCheckDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators {

  property("Generated PolyTransfer Tx should be valid") {
    forAll(validPolyTransferGen) {
      polyTransfer: PolyTransfer =>
        BifrostState.semanticValidity(polyTransfer).isSuccess shouldBe true
    }
  }

  property("Attempting to validate a PolyTransfer without valid signature should error") {
    // Create invalid PolyTransfer
    // send tx to state
    forAll(polyTransferGen) { polyTransfer =>
      BifrostState.semanticValidity(polyTransfer).isSuccess shouldBe false
    }
  }

  property("Attempting to validate a ArbitTransfer without valid signature should error") {
    // Create invalid PolyTransfer
    // send tx to state
    forAll(arbitTransferGen) { arbitTransfer =>
      BifrostState.semanticValidity(arbitTransfer).isSuccess shouldBe false
    }
  }
}
