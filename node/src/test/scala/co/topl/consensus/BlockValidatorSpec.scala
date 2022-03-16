package co.topl.consensus

import co.topl.consensus.NxtConsensus.State
import co.topl.nodeView.history.{BlockProcessor, History}
import co.topl.utils.NodeGenerators
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class BlockValidatorSpec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers with NodeGenerators {

  var history: History = _

  override def beforeAll(): Unit = {
    super.beforeAll()
    history = generateHistory(genesisBlock)
  }

  property("A block with a timestamp older than its parent should never result in a hit") {
    forAll(blockCurve25519Gen) { blockTemp =>
      val block = blockTemp.copy(parentId = history.bestBlockId)
      val nextBlock = block.copy(timestamp = block.timestamp - 1, parentId = block.id)
      val newHistory = history
        .append(
          block,
          NxtConsensus.View(
            NxtConsensus.State(10000000, history.bestBlock.difficulty, 0L, history.bestBlock.height),
            nxtLeaderElection,
            protocolVersioner
          )
        )
        .get
        ._1
      val blockProcessor = BlockProcessor(1024)
      val validator = new DifficultyBlockValidator(newHistory.storage, blockProcessor)
      validator
        .validate(
          nextBlock,
          NxtConsensus.View(
            NxtConsensus.State(10000000, history.bestBlock.difficulty, 0L, history.bestBlock.height),
            nxtLeaderElection,
            protocolVersioner
          )
        )
        .isSuccess shouldBe false
    }
  }
}
