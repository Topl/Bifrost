package co.topl.nodeCodecs.binary.legacy.network.message

import co.topl.codecs.binary.legacy.{BifrostSerializer, Reader, Writer}
import co.topl.network.message.Messages.MessagesV1

class GetPeersSerializer extends BifrostSerializer[MessagesV1.PeersSpecRequest] {

  override def serialize(obj: MessagesV1.PeersSpecRequest, w: Writer): Unit = {}

  override def parse(r: Reader): MessagesV1.PeersSpecRequest = {
    require(r.remaining == 0, "Non-empty data for GetPeers")
    MessagesV1.PeersSpecRequest()
  }
}
