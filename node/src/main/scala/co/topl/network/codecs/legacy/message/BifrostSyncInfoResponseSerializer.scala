package co.topl.network.codecs.legacy.message

import co.topl.codecs.binary.legacy.modifier.ModifierIdSerializer
import co.topl.codecs.binary.legacy.{BifrostSerializer, Reader, Writer}
import co.topl.network.BifrostSyncInfo
import co.topl.network.message.Messages.MessagesV1

object BifrostSyncInfoResponseSerializer extends BifrostSerializer[MessagesV1.BifrostSyncInfoRequest] {

  override def serialize(data: MessagesV1.BifrostSyncInfoRequest, w: Writer): Unit = {
    w.putUShort(data.syncInfo.lastBlockIds.size)
    data.syncInfo.lastBlockIds.foreach(id => w.putBytes(ModifierIdSerializer.toBytes(id)))
  }

  override def parse(r: Reader): MessagesV1.BifrostSyncInfoRequest = {
    val length = r.getUShort()
    val ids = (1 to length).map(_ => ModifierIdSerializer.parse(r))
    MessagesV1.BifrostSyncInfoRequest(BifrostSyncInfo(ids))
  }
}
