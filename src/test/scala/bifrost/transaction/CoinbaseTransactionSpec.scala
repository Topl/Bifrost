package bifrost.transaction

import bifrost.{BifrostGenerators, ValidGenerators}
import bifrost.state.BifrostState
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}

class CoinbaseTransactionSpec extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators {

  property("Generated Coinbase Tx should be valid") {
    forAll(validCoinbaseTransactionGen) {
      cb: CoinbaseTransaction => BifrostState.semanticValidity(cb).isSuccess shouldBe true
    }
  }
}
