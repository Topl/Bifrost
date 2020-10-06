package co.topl.network.message

import co.topl.modifier.ModifierId
import co.topl.modifier.NodeViewModifier.ModifierTypeId
import co.topl.modifier.block.Block


case class BifrostSyncInfo(lastBlockIds: Seq[ModifierId]) extends SyncInfo {

  def startingPoints: Seq[(ModifierTypeId, ModifierId)] = lastBlockIds.map(b => Block.modifierTypeId -> b)

}

object BifrostSyncInfo {
  val MaxLastBlocks = 1000
}
