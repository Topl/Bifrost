package co.topl.network.codecs.legacy.message

import co.topl.codecs.binary.legacy.{BifrostSerializer, Reader, Writer}
import co.topl.network.message.Messages.MessagesV1

class PeersMetadataRequestSerializer extends BifrostSerializer[MessagesV1.PeersMetadataRequest] {

  override def serialize(obj: MessagesV1.PeersMetadataRequest, w: Writer): Unit = {}

  override def parse(r: Reader): MessagesV1.PeersMetadataRequest = {
    require(r.remaining == 0, "Non-empty data for GetPeers")
    MessagesV1.PeersMetadataRequest()
  }
}
