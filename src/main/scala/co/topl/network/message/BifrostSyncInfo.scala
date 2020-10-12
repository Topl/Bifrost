package co.topl.network.message

import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.nodeView.NodeViewModifier.ModifierTypeId


case class BifrostSyncInfo(lastBlockIds: Seq[ModifierId]) extends SyncInfo {

  def startingPoints: Seq[(ModifierTypeId, ModifierId)] = lastBlockIds.map(b => Block.modifierTypeId -> b)

}

object BifrostSyncInfo {
  val MaxLastBlocks = 1000
}
