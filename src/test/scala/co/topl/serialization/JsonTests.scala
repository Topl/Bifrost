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
      box.json.as[PolyBox].right.value shouldBe box
    }
  }

  property("ArbitBox json") {
    forAll(arbitBoxGen) { box =>
      box.json.as[ArbitBox].right.value shouldBe box
    }
  }

  property("AssetBox json") {
    forAll(assetBoxGen) { box =>
      box.json.as[AssetBox].right.value shouldBe box
    }
  }

  property("StateBox json") {
    forAll(stateBoxGen) { box =>
      box.json.as[StateBox].right.value shouldBe box
    }
  }

  property("CodeBox json") {
    forAll(codeBoxGen) { box =>
      box.json.as[CodeBox].right.value shouldBe box
    }
  }

  property("ExecutionBox json") {
    forAll(executionBoxGen) { box =>
      box.json.as[ExecutionBox].right.value shouldBe box
    }
  }

  property("PolyTransfer json") {
    forAll(polyTransferGen) { tx =>
      tx.json.as[PolyTransfer].right.value shouldBe tx
    }
  }
  property("ArbitTransfer json") {
    forAll(arbitTransferGen) { tx =>
      tx.json.as[ArbitTransfer].right.value shouldBe tx
    }
  }

  property("AssetCreation json") {
    forAll(assetCreationGen) { tx =>
      tx.json.as[AssetCreation].right.value shouldBe tx
    }
  }

  property("ProgramCreation json") {
    forAll(programCreationGen) { tx =>
      tx.json.as[ProgramCreation].right.value shouldBe tx
    }
  }

  property("ProgramMethodExecution json") {
    forAll(programMethodExecutionGen) { tx =>
      tx.json.as[ProgramMethodExecution].right.value shouldBe tx
    }
  }

  property("CodeCreation json") {
    forAll(programCreationGen) { tx =>
      tx.json.as[ProgramCreation].right.value shouldBe tx
    }
  }
}
