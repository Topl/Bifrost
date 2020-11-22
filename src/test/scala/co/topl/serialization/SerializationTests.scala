package co.topl.serialization

import co.topl.attestation.ThresholdPropositionCurve25519
import co.topl.attestation.serialization.ThresholdPropositionCurve25519Serializer
import co.topl.modifier.block.{Block, BlockSerializer}
import co.topl.modifier.transaction._
import co.topl.modifier.transaction.serialization._
import co.topl.nodeView.state.box._
import co.topl.nodeView.state.box.serialization.BoxSerializer
import co.topl.program.{ExecutionBuilder, ExecutionBuilderSerializer}
import co.topl.{BifrostGenerators, ValidGenerators}
import org.scalatest.Ignore
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

import scala.collection.BitSet
import scala.util.{Failure, Success}

/**
  * Created by cykoz on 4/12/17.
  */
@Ignore
class SerializationTests extends AnyPropSpec
  with ScalaCheckPropertyChecks
  with ScalaCheckDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators {

  property("oneOfNProposition Serialization") {
    forAll(oneOfNPropositionGen) {
      case (_, mn: ThresholdPropositionCurve25519) =>
        val parsed = ThresholdPropositionCurve25519Serializer
          .parseBytes(ThresholdPropositionCurve25519Serializer.toBytes(mn))
          .get

        parsed.threshold shouldBe mn.threshold
        parsed.setOfPubKeyBytes should contain theSameElementsAs mn.setOfPubKeyBytes
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
        val json = b.json
        val parsed = BoxSerializer
          .parseBytes(BoxSerializer.toBytes(b))
          .get

        val serialized = BoxSerializer.toBytes(parsed)
        json.as[StateBox].right.get.bytes sameElements BoxSerializer.toBytes(b) shouldBe true
        serialized sameElements BoxSerializer.toBytes(b) shouldBe true
    }
  }

  property("CodeBox Serialization") {
    forAll(codeBoxGen) {
      b: CodeBox =>
        val json = b.json
        val parsed = BoxSerializer
          .parseBytes(BoxSerializer.toBytes(b))
          .get

        val serialized = BoxSerializer.toBytes(parsed)
        json.as[CodeBox].right.get.bytes sameElements BoxSerializer.toBytes(b) shouldBe true
        serialized sameElements BoxSerializer.toBytes(b) shouldBe true
    }
  }

  property("ExecutionBox Serialization") {
    forAll(executionBoxGen) {
      b: ExecutionBox =>
        val json = b.json
        val parsed = BoxSerializer
          .parseBytes(BoxSerializer.toBytes(b))
          .get

        val serialized = BoxSerializer.toBytes(parsed)
        json.as[ExecutionBox].right.get.bytes sameElements BoxSerializer.toBytes(b) shouldBe true
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
      sc: PolyTransfer =>
        val parsed = PolyTransferSerializer
          .parseBytes(PolyTransferSerializer.toBytes(sc))
          .get

        PolyTransferSerializer.toBytes(parsed) sameElements
          PolyTransferSerializer.toBytes(sc) shouldBe true
    }
  }

  property("ArbitTransfer Serialization") {
    forAll(arbitTransferGen) {
      ac: ArbitTransfer =>
        val parsed = ArbitTransferSerializer
          .parseBytes(ArbitTransferSerializer.toBytes(ac))
          .get

        ArbitTransferSerializer.toBytes(parsed) sameElements
          ArbitTransferSerializer.toBytes(ac) shouldBe true
    }
  }

  property("AssetTransfer Serialization") {
    forAll(assetTransferGen) {
      at: AssetTransfer =>
        val parsed = AssetTransferSerializer
          .parseBytes(AssetTransferSerializer.toBytes(at))
          .get

        AssetTransferSerializer.toBytes(parsed) sameElements
          AssetTransferSerializer.toBytes(at) shouldBe true
    }
  }

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

  property("AssetCreation Serialization") {
    forAll(assetCreationGen) {
      ac: AssetCreation =>
        val parsed: AssetCreation = AssetCreationSerializer
          .parseBytes(AssetCreationSerializer.toBytes(ac))
          .get

        AssetCreationSerializer.toBytes(parsed) sameElements
          AssetCreationSerializer.toBytes(ac) shouldBe true
    }
  }

  property("CodeCreation Serialization") {
    forAll(codeBoxCreationGen) {
      ccc: CodeCreation =>
        val parsed = CodeBoxCreationSerializer
          .parseBytes(CodeBoxCreationSerializer.toBytes(ccc))
          .get

        CodeBoxCreationSerializer.toBytes(parsed) sameElements
          CodeBoxCreationSerializer.toBytes(ccc) shouldBe true
    }
  }

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

  //TODO Test after all txs and state tests work
  property("Block Serialization") {
    forAll(BlockGen) {
      bb: Block =>
        val parsed = BlockSerializer.parseBytes(BlockSerializer.toBytes(bb))

        parsed match {
          case Success(p) => BlockSerializer.toBytes(p) sameElements
            BlockSerializer.toBytes(bb) shouldBe true
          case Failure(e) => throw e
        }
    }
  }


  // todo: JAA - 2020.08.02 - this was removed as SyncInfo uses the standard message parsing pattern now.
  // todo:       We should be sure to move this test to where ever message serialization is tested
//  property("BifrostSyncInfo Serialization") {
//    forAll(bifrostSyncInfoGen) {
//      syncInfo: BifrostSyncInfo =>
//        val parsed = BifrostSyncInfoSerializer
//          .parseBytes(BifrostSyncInfoSerializer.toBytes(syncInfo))
//          .get
//
//        BifrostSyncInfoSerializer.toBytes(parsed) sameElements
//          BifrostSyncInfoSerializer.toBytes(syncInfo) shouldBe true
//    }
//  }

  property("Bloom Serialization") {
    forAll(intSeqGen) {
      intSeq: Seq[Int] =>
        val bloom = BloomFilter(BitSet(intSeq: _*))
        val parsed: BloomFilter = BloomFilterSerializer
          .parseBytes(BloomFilterSerializer.toBytes(bloom))
          .get

        BloomFilterSerializer.toBytes(parsed) sameElements
          BloomFilterSerializer.toBytes(bloom) shouldBe true
    }
  }
}
