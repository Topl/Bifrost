package bifrost.consensus

import bifrost.BifrostGenerators
import bifrost.history.History
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.concurrent.duration.FiniteDuration

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
      val blockTime = FiniteDuration(1, "second")
      val validator = new DifficultyBlockValidator(newHistory.storage)
      validator.validate(nextBlock).isSuccess shouldBe false
    }
  }
}
