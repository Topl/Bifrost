package co.topl.nodeCodecs.binary.legacy.network.message

import co.topl.codecs.binary.legacy.modifier.ModifierIdSerializer
import co.topl.codecs.binary.legacy.{BifrostSerializer, Reader, Writer}
import co.topl.network.BifrostSyncInfo
import co.topl.network.message.Messages.MessagesV1

object BifrostSyncInfoResponseSerializer extends BifrostSerializer[MessagesV1.BifrostSyncInfoResponse] {

  override def serialize(data: MessagesV1.BifrostSyncInfoResponse, w: Writer): Unit = {
    w.putUShort(data.syncInfo.lastBlockIds.size)
    data.syncInfo.lastBlockIds.foreach(id => w.putBytes(ModifierIdSerializer.toBytes(id)))
  }

  override def parse(r: Reader): MessagesV1.BifrostSyncInfoResponse = {
    val length = r.getUShort()
    val ids = (1 to length).map(_ => ModifierIdSerializer.parse(r))
    MessagesV1.BifrostSyncInfoResponse(BifrostSyncInfo(ids))
  }
}
