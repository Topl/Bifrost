package co.topl.consensus

import co.topl.consensus.consensusHelper.setProtocolMngr
import co.topl.nodeView.history.{BlockProcessor, History}
import co.topl.utils.{CoreGenerators, NodeGenerators}
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class BlockValidatorSpec
    extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with Matchers
    with CoreGenerators
    with NodeGenerators {

  var history: History = _

  override def beforeAll(): Unit = {
    super.beforeAll()

    /* Initialize protocolMngr */
    setProtocolMngr(settings)

    history = generateHistory()
  }

  property("A block with a timestamp older than its parent should never result in a hit") {
    forAll(blockGen) { blockTemp =>
      val block = blockTemp.copy(parentId = history.bestBlockId)
      val nextBlock = block.copy(timestamp = block.timestamp - 1, parentId = block.id)
      val newHistory = history.append(block).get._1
      val blockProcessor = BlockProcessor(1024)
      val validator = new DifficultyBlockValidator(newHistory.storage, blockProcessor)
      validator.validate(nextBlock).isSuccess shouldBe false
    }
  }
}
