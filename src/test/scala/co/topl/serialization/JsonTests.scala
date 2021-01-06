package co.topl.serialization

import co.topl.attestation.Proposition
import co.topl.modifier.transaction.{ArbitTransfer, AssetTransfer, PolyTransfer}
import co.topl.nodeView.state.box._
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
      box.asJson.as[PolyBox] shouldEqual Right(box)
    }
  }

  property("ArbitBox json") {
    forAll(arbitBoxGen) { box =>
      box.asJson.as[ArbitBox] shouldEqual Right(box)
    }
  }

  property("AssetBox json") {
    forAll(assetBoxGen) { box =>
      box.asJson.as[AssetBox] shouldEqual Right(box)
    }
  }

  property("StateBox json") {
    forAll(stateBoxGen) { box =>
      box.asJson.as[StateBox] shouldEqual Right(box)
    }
  }

  property("CodeBox json") {
    forAll(codeBoxGen) { box =>
      box.asJson.as[CodeBox] shouldEqual Right(box)
    }
  }

  property("ExecutionBox json") {
    forAll(executionBoxGen) { box =>
      box.asJson.as[ExecutionBox] shouldEqual Right(box)
    }
  }

  property("PolyTransfer json") {
    forAll(polyTransferGen) { tx =>
      tx.asJson.as[PolyTransfer[_ <: Proposition]] shouldEqual Right(tx)
    }
  }

  property("ArbitTransfer json") {
    forAll(arbitTransferGen) { tx =>
      tx.asJson.as[ArbitTransfer[_ <: Proposition]] shouldEqual Right(tx)
    }
  }

  property("AssetTransfer json") {
    forAll(assetTransferGen) { tx =>
      tx.asJson.as[AssetTransfer[_ <: Proposition]] shouldEqual Right(tx)
    }
  }
}
