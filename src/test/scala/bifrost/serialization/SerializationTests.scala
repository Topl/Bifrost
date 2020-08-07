package bifrost.serialization

import bifrost.modifier.block.{Block, BlockCompanion}
import bifrost.program.{ExecutionBuilder, ExecutionBuilderCompanion}
import bifrost.modifier.transaction.bifrostTransaction._
import bifrost.modifier.box._
import bifrost.modifier.box.proposition.{MofNProposition, MofNPropositionSerializer}
import bifrost.modifier.transaction.serialization._
import bifrost.{BifrostGenerators, ValidGenerators}
import bifrost.network.{BifrostSyncInfo, BifrostSyncInfoSerializer}
import serializer.BloomTopics

import scala.collection.BitSet
import scala.util.{Failure, Success}
import org.scalatestplus.scalacheck.{ ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks }
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

/**
  * Created by cykoz on 4/12/17.
  */
class SerializationTests extends AnyPropSpec
  with ScalaCheckPropertyChecks
  with ScalaCheckDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators {

  property("oneOfNProposition Serialization") {
    forAll(oneOfNPropositionGen) {
      case (_, mn: MofNProposition) =>
        val parsed = MofNPropositionSerializer
          .parseBytes(MofNPropositionSerializer.toBytes(mn))
          .get

        parsed.m shouldBe mn.m
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
        val parsed = ExecutionBuilderCompanion
          .parseBytes(ExecutionBuilderCompanion.toBytes(a))
          .get

        ExecutionBuilderCompanion.toBytes(parsed) sameElements ExecutionBuilderCompanion.toBytes(a) shouldBe true
    }
  }

  property("PolyTransfer Serialization") {
    forAll(polyTransferGen) {
      sc: PolyTransfer =>
        val parsed = TransferTransactionCompanion
          .parseBytes(TransferTransactionCompanion.toBytes(sc))
          .get

        TransferTransactionCompanion.toBytes(parsed) sameElements
          TransferTransactionCompanion.toBytes(sc) shouldBe true
    }
  }

  property("ArbitTransfer Serialization") {
    forAll(arbitTransferGen) {
      ac: ArbitTransfer =>
        val parsed = TransferTransactionCompanion
          .parseBytes(TransferTransactionCompanion.toBytes(ac))
          .get

        TransferTransactionCompanion.toBytes(parsed) sameElements
          TransferTransactionCompanion.toBytes(ac) shouldBe true
    }
  }

  property("AssetTransfer Serialization") {
    forAll(assetTransferGen) {
      at: AssetTransfer =>
        val parsed = TransferTransactionCompanion
          .parseBytes(TransferTransactionCompanion.toBytes(at))
          .get

        TransferTransactionCompanion.toBytes(parsed) sameElements
          TransferTransactionCompanion.toBytes(at) shouldBe true
    }
  }

  property("ProgramCreation Serialization") {
    forAll(programCreationGen) {
      c: ProgramCreation =>
        val parsed = ProgramTransactionCompanion
          .parseBytes(ProgramTransactionCompanion.toBytes(c))
          .get

        val parsedBytes = ProgramTransactionCompanion.toBytes(parsed)
        val directParsedBytes = ProgramTransactionCompanion.toBytes(c)

        c.executionBuilder shouldEqual parsed.asInstanceOf[ProgramCreation].executionBuilder
        c.json shouldEqual parsed.asInstanceOf[ProgramCreation].json

        parsedBytes sameElements directParsedBytes shouldBe true
    }
  }

  property("ProgramMethodExecution Serialization") {
    forAll(programMethodExecutionGen) {
      c: ProgramMethodExecution =>
        val parsed = ProgramTransactionCompanion
          .parseBytes(ProgramTransactionCompanion.toBytes(c))
          .get

        ProgramTransactionCompanion.toBytes(parsed) sameElements
          ProgramTransactionCompanion.toBytes(c) shouldBe true
    }
  }

  property("AssetCreation Serialization") {
    forAll(assetCreationGen) {
      ac: AssetCreation =>
        val parsed: AssetCreation = AssetCreationCompanion
          .parseBytes(AssetCreationCompanion.toBytes(ac))
          .get

        AssetCreationCompanion.toBytes(parsed) sameElements
          AssetCreationCompanion.toBytes(ac) shouldBe true
    }
  }

  property("AssetRedemption Serialization") {
    forAll(assetRedemptionGen) {
      ar: AssetRedemption =>
        val parsed = AssetRedemptionCompanion
          .parseBytes(AssetRedemptionCompanion.toBytes(ar))
          .get

        AssetRedemptionCompanion.toBytes(parsed) sameElements
          AssetRedemptionCompanion.toBytes(ar) shouldBe true
    }
  }

  property("CodeCreation Serialization") {
    forAll(codeBoxCreationGen) {
      ccc: CodeCreation =>
        val parsed = CodeBoxCreationCompanion
          .parseBytes(CodeBoxCreationCompanion.toBytes(ccc))
          .get

        CodeBoxCreationCompanion.toBytes(parsed) sameElements
          CodeBoxCreationCompanion.toBytes(ccc) shouldBe true
    }
  }

  property("ProgramTransfer Serialization") {
    forAll(programTransferGen) {
      pt: ProgramTransfer =>
        val parsed = ProgramTransferCompanion
          .parseBytes(ProgramTransferCompanion.toBytes(pt))
          .get

        ProgramTransferCompanion.toBytes(parsed) sameElements
          ProgramTransferCompanion.toBytes(pt) shouldBe true
    }
  }

  //TODO Test after all txs and state tests work
  property("Block Serialization") {
    forAll(BlockGen) {
      bb: Block =>
        val parsed = BlockCompanion.parseBytes(BlockCompanion.toBytes(bb))

        parsed match {
          case Success(p) => BlockCompanion.toBytes(p) sameElements
            BlockCompanion.toBytes(bb) shouldBe true
          case Failure(e) => throw e
        }
    }
  }

  property("BifrostSyncInfo Serialization") {
    forAll(bifrostSyncInfoGen) {
      syncInfo: BifrostSyncInfo =>
        val parsed = BifrostSyncInfoSerializer
          .parseBytes(BifrostSyncInfoSerializer.toBytes(syncInfo))
          .get

        BifrostSyncInfoSerializer.toBytes(parsed) sameElements
          BifrostSyncInfoSerializer.toBytes(syncInfo) shouldBe true
    }
  }

  property("Bloom Serialization") {
    forAll(intSeqGen) {
      intSeq: Seq[Int] =>
        val parsed = BitSet() ++ BloomTopics
          .parseFrom(BloomTopics(intSeq).toByteArray)
          .topics

        BloomTopics(parsed.toSeq).toByteArray sameElements
          BloomTopics((BitSet() ++ intSeq).toSeq).toByteArray shouldBe true
    }
  }
}
