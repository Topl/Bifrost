package co.topl.consensus

import co.topl.BifrostGenerators
import co.topl.nodeView.history.{BlockProcessor, History}
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class BlockValidatorSpec extends AnyPropSpec
  with ScalaCheckPropertyChecks
  with Matchers
  with BifrostGenerators {

  val history: History = generateHistory

  property("A block with a timestamp older than its parent should never result in a hit") {
    forAll(BlockGen) { blockTemp ⇒
      val block = blockTemp.copy(parentId = history.bestBlockId)
      val nextBlock = block.copy(timestamp = block.timestamp - 1, parentId = block.id)
      val newHistory = history.append(block).get._1
      val blockProcessor = BlockProcessor(1024)
      val validator = new DifficultyBlockValidator(newHistory.storage, blockProcessor)
      validator.validate(nextBlock).isSuccess shouldBe false
    }
  }
}
