package co.topl.serialization

import co.topl.attestation.serialization.ThresholdPropositionCurve25519Serializer
import co.topl.attestation.{Address, Proposition, ThresholdPropositionCurve25519}
import co.topl.modifier.block.serialization.BlockSerializer
import co.topl.modifier.block.{Block, BloomFilter}
import co.topl.modifier.transaction._
import co.topl.modifier.transaction.serialization._
import co.topl.nodeView.state.box._
import co.topl.nodeView.state.box.serialization.BoxSerializer
import co.topl.program.{ExecutionBuilder, ExecutionBuilderSerializer}
import co.topl.utils.{CoreGenerators, ValidGenerators}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

import scala.util.{Failure, Success}

/**
  * Created by cykoz on 4/12/17.
  */
class SerializationTests extends AnyPropSpec
  with ScalaCheckPropertyChecks
  with ScalaCheckDrivenPropertyChecks
  with Matchers
  with CoreGenerators
  with ValidGenerators {

  property("oneOfNProposition Serialization") {
    forAll(oneOfNPropositionGen) {
      case (_, mn: ThresholdPropositionCurve25519) =>
        val parsed = ThresholdPropositionCurve25519Serializer
          .parseBytes(ThresholdPropositionCurve25519Serializer.toBytes(mn))
          .get

        parsed.threshold shouldBe mn.threshold
        parsed.pubKeyProps should contain theSameElementsAs mn.pubKeyProps
    }
  }

  property("PolyBox Serialization") {
    forAll(polyBoxGen) {
      b: PolyBox =>
        val parsed = BoxSerializer
          .parseBytes(BoxSerializer.toBytes(b))
          .get

        val serialized = BoxSerializer.toBytes(parsed)
        serialized sameElements BoxSerializer.toBytes(b) shouldBe true
    }
  }

  property("ArbitBox Serialization") {
    forAll(arbitBoxGen) {
      b: ArbitBox =>
        val parsed = BoxSerializer
          .parseBytes(BoxSerializer.toBytes(b))
          .get

        val serialized = BoxSerializer.toBytes(parsed)
        serialized sameElements BoxSerializer.toBytes(b) shouldBe true
    }
  }

  property("AssetBox Serialization") {
    forAll(assetBoxGen) {
      b: AssetBox =>
        val parsed = BoxSerializer
          .parseBytes(BoxSerializer.toBytes(b))
          .get

        val serialized = BoxSerializer.toBytes(parsed)
        serialized sameElements BoxSerializer.toBytes(b) shouldBe true
    }
  }

  property("StateBox Serialization") {
    forAll(stateBoxGen) {
      b: StateBox =>
        val parsed = BoxSerializer
          .parseBytes(BoxSerializer.toBytes(b))
          .get

        val serialized = BoxSerializer.toBytes(parsed)
        serialized sameElements BoxSerializer.toBytes(b) shouldBe true
    }
  }

  property("CodeBox Serialization") {
    forAll(codeBoxGen) {
      b: CodeBox =>
        val parsed = BoxSerializer
          .parseBytes(BoxSerializer.toBytes(b))
          .get

        val serialized = BoxSerializer.toBytes(parsed)
        serialized sameElements BoxSerializer.toBytes(b) shouldBe true
    }
  }

  property("ExecutionBox Serialization") {
    forAll(executionBoxGen) {
      b: ExecutionBox =>
        val parsed = BoxSerializer
          .parseBytes(BoxSerializer.toBytes(b))
          .get

        val serialized = BoxSerializer.toBytes(parsed)
        serialized sameElements BoxSerializer.toBytes(b) shouldBe true
    }
  }

  property("ExecutionBuilder Serialization") {
    forAll(validExecutionBuilderGen()) {
      a: ExecutionBuilder =>
        val parsed = ExecutionBuilderSerializer
          .parseBytes(ExecutionBuilderSerializer.toBytes(a))
          .get

        ExecutionBuilderSerializer.toBytes(parsed) sameElements ExecutionBuilderSerializer.toBytes(a) shouldBe true
    }
  }

  property("PolyTransfer Serialization") {
    forAll(polyTransferGen) {
      tx: PolyTransfer[_ <: Proposition] =>
        val serializer = TransactionSerializer
        val parsed = serializer
          .parseBytes(serializer.toBytes(tx))
          .get

        serializer.toBytes(parsed) sameElements
          serializer.toBytes(tx) shouldBe true
    }
  }

  property("ArbitTransfer Serialization") {
    forAll(arbitTransferGen) {
      tx: ArbitTransfer[_ <: Proposition] =>
        val serializer = TransactionSerializer
        val parsed = serializer
          .parseBytes(serializer.toBytes(tx))
          .get

        serializer.toBytes(parsed) sameElements
          serializer.toBytes(tx) shouldBe true
    }
  }

  property("AssetTransfer Serialization") {
    forAll(assetTransferGen) {
      tx: AssetTransfer[_ <: Proposition] =>
        val serializer = TransactionSerializer
        val parsed = serializer
          .parseBytes(serializer.toBytes(tx))
          .get

        serializer.toBytes(parsed) sameElements
          serializer.toBytes(tx) shouldBe true
    }
  }

  /*
  property("ProgramCreation Serialization") {
    forAll(programCreationGen) {
      c: ProgramCreation =>
        val parsed = ProgramCreationSerializer
          .parseBytes(ProgramCreationSerializer.toBytes(c))
          .get

        val parsedBytes = ProgramCreationSerializer.toBytes(parsed)
        val directParsedBytes = ProgramCreationSerializer.toBytes(c)

        c.executionBuilder shouldEqual parsed.asInstanceOf[ProgramCreation].executionBuilder
        c.json shouldEqual parsed.asInstanceOf[ProgramCreation].json

        parsedBytes sameElements directParsedBytes shouldBe true
    }
  }
  */

  /*
  property("ProgramMethodExecution Serialization") {
    forAll(programMethodExecutionGen) {
      c: ProgramMethodExecution =>
        val parsed = ProgramMethodExecutionSerializer
          .parseBytes(ProgramMethodExecutionSerializer.toBytes(c))
          .get

        ProgramMethodExecutionSerializer.toBytes(parsed) sameElements
          ProgramMethodExecutionSerializer.toBytes(c) shouldBe true
    }
  }
   */

  /*
  property("ProgramTransfer Serialization") {
    forAll(programTransferGen) {
      pt: ProgramTransfer =>
        val parsed = ProgramTransferSerializer
          .parseBytes(ProgramTransferSerializer.toBytes(pt))
          .get

        ProgramTransferSerializer.toBytes(parsed) sameElements
          ProgramTransferSerializer.toBytes(pt) shouldBe true
    }
  }
   */

  property("Block Serialization") {
    forAll(blockGen) {
      bb: Block =>
        val parsed = BlockSerializer.parseBytes(BlockSerializer.toBytes(bb))

        parsed match {
          case Success(p) => BlockSerializer.toBytes(p) sameElements
            BlockSerializer.toBytes(bb) shouldBe true
          case Failure(e) => throw e
        }
    }
  }

  property("Bloom Serialization") {
    forAll(blockGen) {
      block =>
        val parsed: BloomFilter = BloomFilter.parseBytes(BloomFilter.toBytes(block.bloomFilter)).get

        BloomFilter.toBytes(parsed) sameElements
          BloomFilter.toBytes(block.bloomFilter) shouldBe true
    }
  }

  property("Address and Evidence Serialization") {
    forAll(addressGen) {
      address =>
        val parsed: Address = Address.parseBytes(Address.toBytes(address)).get

        Address.toBytes(parsed) sameElements Address.toBytes(address) shouldBe true
    }
  }
}
