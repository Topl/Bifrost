package bifrost.network.message

import bifrost.modifier.ModifierId
import bifrost.modifier.block.Block
import bifrost.nodeView.NodeViewModifier.ModifierTypeId

case class BifrostSyncInfo(lastBlockIds: Seq[ModifierId]) extends SyncInfo {

  def startingPoints: Seq[(ModifierTypeId, ModifierId)] = lastBlockIds.map(b => Block.modifierTypeId -> b)

}

object BifrostSyncInfo {
  val MaxLastBlocks = 1000
}
