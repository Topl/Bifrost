package scorex.testkit.properties

import bifrost.PersistentNodeViewModifier
import bifrost.consensus.{History, SyncInfo}
import bifrost.transaction.Transaction
import bifrost.transaction.box.Box
import bifrost.transaction.box.proposition.Proposition
import bifrost.transaction.state.{MinimalState, StateChanges}
import scorex.testkit.TestkitHelpers

trait StateChangesGenerationTest[P <: Proposition,
TX <: Transaction[P],
PM <: PersistentNodeViewModifier[P, TX],
B <: Box[P],
ST <: MinimalState[P, B, TX, PM, ST],
SI <: SyncInfo,
HT <: History[P, TX, PM, SI, HT]] extends StateTests[P, TX, PM, B, ST] with TestkitHelpers {

  val history: HT

  def genValidModifier(history: HT): PM

  property("State should be able to generate changes from block and apply them") {
    check { _ =>
      val block = genValidModifier(history)
      val blockChanges = state.changes(block).get
      val existingBoxIds = blockChanges.boxIdsToRemove.filter(bi => state.closedBox(bi).isDefined)
      val changes: StateChanges[P, B] = blockChanges.copy(boxIdsToRemove = existingBoxIds)
      val newState = state.applyChanges(changes, block.id).get
      changes.toAppend.foreach { b =>
        newState.closedBox(b.id).isDefined shouldBe true
      }
      changes.boxIdsToRemove.foreach { bId =>
        newState.closedBox(bId).isDefined shouldBe false
      }
    }
  }


}
