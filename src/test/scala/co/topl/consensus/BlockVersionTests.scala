package co.topl.consensus

import co.topl.BifrostGenerators
import co.topl.consensus.consensus.setProtocolMngr
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.nodeView.history.History
import co.topl.nodeView.state.State
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec


class BlockVersionTests extends AnyPropSpec
  with Matchers
  with BifrostGenerators {

  setProtocolMngr(settings)

  var history: History = generateHistory(0: Byte)
  var state: State = State.readOrGenerate(settings)

  val numOfBlocks: Int = 10

  for (_ <- 1 to numOfBlocks) {
    val oneBlock: Block = BlockGen.sample.get.copy(parentId = history.bestBlockId, transactions = Seq(), version = blockVersion(history.height + 1))
    history = history.append(oneBlock).get._1
    state = state.applyModifier(oneBlock).get
  }

  property("Reading a history with different versions of block should yield correct blocks") {
    var currentId: ModifierId = history.storage.bestBlockId
    for (_ <- 0 to numOfBlocks) {
      val currentBlock: Block = history.storage.modifierById(currentId).get
      currentId = currentBlock.parentId
      log.debug(s"${Console.MAGENTA}${currentBlock.json}${Console.RESET}")
    }
  }
}
