package co.topl.modifier.transaction

import co.topl.utils.{CoreGenerators, ValidGenerators}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

class PolyTransferSpec
    extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with Matchers
    with CoreGenerators
    with ValidGenerators {

  property("Generated PolyTransfer Tx should be valid") {
    forAll(validPolyTransfer(keyRing, genesisState)) { polyTransfer: PolyTransfer[_] =>
      polyTransfer.syntacticValidate.toEither match {
        case Left(exception) => println(exception)
        case _               =>
      }
      polyTransfer.syntacticValidate.isValid shouldBe true
    }
  }

  property("Attempting to validate a PolyTransfer without valid signature should error") {
    // Create invalid PolyTransfer
    // send tx to state
    forAll(polyTransferGen) { polyTransfer: PolyTransfer[_] =>
      polyTransfer.syntacticValidate.isValid shouldBe false
    }
  }
}
