package co.topl.network

import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.nodeView.history.GenericHistory.TypedModifierIds

case class BifrostSyncInfo(lastBlockIds: Seq[ModifierId]) extends SyncInfo {

  /** Sequence of modifier ids and type ids */
  override def startingPoints: TypedModifierIds = lastBlockIds.map(Block.modifierTypeId -> _)
}

object BifrostSyncInfo {
  val maxLastBlocks = 1000
}
