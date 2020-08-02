package bifrost.network.message

import bifrost.modifier.ModifierId
import bifrost.modifier.block.Block
import bifrost.nodeView.NodeViewModifier
import bifrost.nodeView.NodeViewModifier.{ModifierTypeId, bytesToId}
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}


case class BifrostSyncInfo(lastBlockIds: Seq[ModifierId]) extends SyncInfo {

  override def startingPoints: Seq[(ModifierTypeId, ModifierId)] =
    lastBlockIds.map(b => Block.modifierTypeId -> b)

  override type M = BifrostSyncInfo

  override def serializer: BifrostSerializer[BifrostSyncInfo] = BifrostSyncInfoSerializer
}

object BifrostSyncInfo {
  val MaxLastBlocks = 1000
}

object BifrostSyncInfoSerializer extends BifrostSerializer[BifrostSyncInfo] {

  override def serialize(obj: BifrostSyncInfo, w: Writer): Unit = {
    w.putUShort(obj.lastBlockIds.size)
    obj.lastBlockIds.foreach(id ⇒ w.putBytes(id.hashBytes))
  }

  override def parse(r: Reader): BifrostSyncInfo = {
    val length = r.getUShort()
    val ids = (1 to length).map(_ ⇒ bytesToId(r.getBytes(NodeViewModifier.ModifierIdSize)))
    BifrostSyncInfo(ids)
  }
}

object BifrostSyncInfoMessageSpec extends SyncInfoMessageSpec[BifrostSyncInfo](BifrostSyncInfoSerializer)
