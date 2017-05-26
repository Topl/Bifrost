package bifrost.transaction

import bifrost.{BifrostGenerators, ValidGenerators}
import bifrost.state.BifrostState
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}

class ArbitTransferSpec extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators {

  property("Generated ArbitTransfer Tx should be valid") {
    forAll(validArbitTransferGen) {
      at: ArbitTransfer =>
        BifrostState.semanticValidity(at).isSuccess shouldBe true
    }
  }
}
