package co.topl.consensus

import co.topl.modifier.block.Block
import co.topl.nodeView.{NodeViewTestHelpers, ValidTransactionGenerators}
import co.topl.nodeView.history.TineProcessor
import co.topl.utils.NodeGenerators
import org.scalatest.matchers.must.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class BlockValidatorSpec
    extends AnyPropSpec
    with ScalaCheckDrivenPropertyChecks
    with Matchers
    with ValidTransactionGenerators
    with NodeGenerators
    with NodeViewTestHelpers {

  property("A properly formed blocked should pass all checks") {
    forAll(nxtConsensusGenesisGen) { genesis =>
      val testIn = genesisNodeViewTestInputs(genesis)
      val nextBlock = nextBlock(testIn.nodeView.history.bestBlock, testIn.nodeView)

      val validators = Seq(
        new DifficultyBlockValidator(testIn.genesisViewleaderElection, NxtConsensus.State.empty),
        new SyntaxBlockValidator(NxtConsensus.State.empty),
        new TimestampValidator
      )


    }
  }
  property("A randomly generated block should fail the syntax check") {}
  property("A block with non-reproducible difficulty should not validate") {
    forAll(genesisBlockGen) { genesisBlock =>
      val history = generateHistory(genesisBlock)

      val block = blockTemp.copy(parentId = history.bestBlockId)
      val nextBlock = block.copy(timestamp = block.timestamp - 1, parentId = block.id)
      val newHistory = history
        .append(
          block,
          NxtConsensus.View(
            NxtConsensus.State(10000000, history.bestBlock.difficulty, 0L, history.bestBlock.height),
            testLeaderElection,
            testProtocolVersioner
          )
        )
        .get
        ._1

      val tineProcessor = TineProcessor(1024)

      val validator = new DifficultyBlockValidator(newHistory.storage, tineProcessor)

      validator
        .validate(
          nextBlock,
          NxtConsensus.View(
            NxtConsensus.State(10000000, history.bestBlock.difficulty, 0L, history.bestBlock.height),
            testLeaderElection,
            testProtocolVersioner
          )
        )
        .isSuccess shouldBe false
    }
  }
  property("A block with a timestamp younger than its parent should fail the timestamp check") {}

}
