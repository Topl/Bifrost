package co.topl.serialization

import co.topl.utils.{CoreGenerators, ValidGenerators}
import io.circe.syntax.EncoderOps
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class JsonTests extends AnyPropSpec
  with Matchers
  with ScalaCheckPropertyChecks
  with CoreGenerators
  with ValidGenerators {

  property("PolyBox json") {
    forAll(polyBoxGen) { box =>
      box.asJson shouldEqual Right(box)
    }
  }

  property("ArbitBox json") {
    forAll(arbitBoxGen) { box =>
      box.asJson shouldEqual Right(box)
    }
  }

  property("AssetBox json") {
    forAll(assetBoxGen) { box =>
      box.asJson shouldEqual Right(box)
    }
  }

  property("StateBox json") {
    forAll(stateBoxGen) { box =>
      box.asJson shouldEqual Right(box)
    }
  }

  property("CodeBox json") {
    forAll(codeBoxGen) { box =>
      box.asJson shouldEqual Right(box)
    }
  }

  property("ExecutionBox json") {
    forAll(executionBoxGen) { box =>
      box.asJson shouldEqual Right(box)
    }
  }

/*
  property("PolyTransfer json") {
    forAll(polyTransferGen) { tx =>
      tx.asJson shouldEqual Right(tx)
    }
  }

  property("ArbitTransfer json") {
    forAll(arbitTransferGen) { tx =>
      tx.asJson shouldEqual Right(tx)
    }
  }

  property("AssetTransfer json") {
    forAll(assetTransferGen) { tx =>
      tx.asJson shouldEqual Right(tx)
    }
  }

  property("AssetCreation json") {
    forAll(assetCreationGen) { tx =>
      tx.asJson shouldEqual Right(tx)
    }
  }

  property("ProgramCreation json") {
    forAll(programCreationGen) { tx =>
      tx.asJson shouldEqual Right(tx)
    }
  }

  property("ProgramMethodExecution json") {
    forAll(programMethodExecutionGen) { tx =>
      tx.asJson shouldEqual Right(tx)
    }
  }

  property("CodeCreation json") {
    forAll(programCreationGen) { tx =>
      tx.asJson shouldEqual Right(tx)
    }
  }
*/
}
