package co.topl.network.message

import co.topl.modifier.ModifierId
import co.topl.modifier.NodeViewModifier.ModifierTypeId
import co.topl.modifier.block.Block

/** This case class contains ids of a sequence of modifiers, that node with info should download and apply
  * to synchronize
  *
  * @param lastBlockIds is the ids of a sequence of modifiers, from older blocks to the best block
  */
case class BifrostSyncInfo(lastBlockIds: Seq[ModifierId]) extends SyncInfo {

  /** LastBlockIds zipped with the modifier type id of the block(should be 3 as a byte) */
  def startingPoints: Seq[(ModifierTypeId, ModifierId)] =
    lastBlockIds.map(b => Block.modifierTypeId -> b)
}

object BifrostSyncInfo {
  val MaxLastBlocks = 1000
}
