package co.topl.serialization

import co.topl.attestation.{Address, Evidence, Proposition, PublicKeyPropositionCurve25519}
import co.topl.modifier.ModifierId
import co.topl.modifier.block.{Block, BlockBody, BlockHeader}
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

  property("PublicKey25519Proposition json") {
    forAll(propositionGen) { prop =>
      prop.asJson.as[PublicKeyPropositionCurve25519] shouldEqual Right(prop)
    }
  }

  property("Address json") {
    forAll(addressGen) { address =>
      address.asJson.as[Address] shouldEqual Right(address)
    }
  }

  property("Evidence json") {
    forAll(evidenceGen) { evidence =>
      evidence.asJson.as[Evidence] shouldEqual Right(evidence)
    }
  }

  property("ModifierId json") {
    forAll(modifierIdGen) { id =>
      id.asJson.as[ModifierId] shouldEqual Right(id)
    }
  }

  property("AssetCode json") {
    forAll(assetCodeGen) { code =>
      code.asJson.as[AssetCode] shouldEqual Right(code)
    }
  }

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

  property("ProgramId json") {
    forAll(programIdGen) { id =>
      id.asJson.as[ProgramId] shouldEqual Right(id)
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

  property("FullBlock json") {
    forAll(blockGen) { block =>
      block.asJson.as[Block] shouldEqual Right(block)
    }
  }

  property("BlockHeader json") {
    forAll(blockGen) { block =>
      block.toComponents._1.asJson.as[BlockHeader] shouldEqual Right(block.toComponents._1)
    }
  }

  property("BlockBody json") {
    forAll(blockGen) { block =>
      block.toComponents._2.asJson.as[BlockBody] shouldEqual Right(block.toComponents._2)
    }
  }
}
