package co.topl.serialization

import akka.util.Helpers.Requiring
import co.topl.modifier.transaction.{ArbitTransfer, AssetCreation, PolyTransfer, ProgramCreation, ProgramMethodExecution}
import co.topl.nodeView.state.box.{ArbitBox, AssetBox, CodeBox, ExecutionBox, PolyBox, StateBox}
import co.topl.utils.{CoreGenerators, ValidGenerators}
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
      box.json.as[PolyBox].value shouldEqual Right(box)
    }
  }

  property("ArbitBox json") {
    forAll(arbitBoxGen) { box =>
      box.json.as[ArbitBox].value shouldEqual Right(box)
    }
  }

  property("AssetBox json") {
    forAll(assetBoxGen) { box =>
      box.json.as[AssetBox].value shouldEqual Right(box)
    }
  }

  property("StateBox json") {
    forAll(stateBoxGen) { box =>
      box.json.as[StateBox].value shouldEqual Right(box)
    }
  }

  property("CodeBox json") {
    forAll(codeBoxGen) { box =>
      box.json.as[CodeBox].value shouldEqual Right(box)
    }
  }

  property("ExecutionBox json") {
    forAll(executionBoxGen) { box =>
      box.json.as[ExecutionBox].value shouldEqual Right(box)
    }
  }

/*
  property("PolyTransfer json") {
    forAll(polyTransferGen) { tx =>
      tx.json.as[PolyTransfer].value shouldEqual Right(tx)
    }
  }

  property("ArbitTransfer json") {
    forAll(arbitTransferGen) { tx =>
      tx.json.as[ArbitTransfer].value shouldEqual Right(tx)
    }
  }

  property("AssetTransfer json") {
    forAll(assetTransferGen) { tx =>
      tx.json.as[ArbitTransfer].value shouldEqual Right(tx)
    }
  }

  property("AssetCreation json") {
    forAll(assetCreationGen) { tx =>
      tx.json.as[AssetCreation].value shouldEqual Right(tx)
    }
  }

  property("ProgramCreation json") {
    forAll(programCreationGen) { tx =>
      tx.json.as[ProgramCreation].value shouldEqual Right(tx)
    }
  }

  property("ProgramMethodExecution json") {
    forAll(programMethodExecutionGen) { tx =>
      tx.json.as[ProgramMethodExecution].value shouldEqual Right(tx)
    }
  }

  property("CodeCreation json") {
    forAll(programCreationGen) { tx =>
      tx.json.as[ProgramCreation].value shouldEqual Right(tx)
    }
  }
*/
}
