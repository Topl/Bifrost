package co.topl.network.codecs.legacy.message

import co.topl.codecs.binary.legacy.{BifrostSerializer, Reader, Writer}
import co.topl.network.codecs.legacy.peer.PeerSpecSerializer
import co.topl.network.message.Messages.MessagesV1
import co.topl.network.peer.PeerFeature
import co.topl.utils.Extensions.LongOps

class PeersSerializer(featureSerializers: PeerFeature.Serializers, peersLimit: Int)
    extends BifrostSerializer[MessagesV1.PeersSpecResponse] {

  private val peerSpecSerializer = new PeerSpecSerializer(featureSerializers)

  override def serialize(data: MessagesV1.PeersSpecResponse, w: Writer): Unit = {
    w.putUInt(data.peers.size)
    data.peers.foreach(p => peerSpecSerializer.serialize(p, w))
  }

  override def parse(r: Reader): MessagesV1.PeersSpecResponse = {
    val length = r.getUInt().toIntExact
    require(length <= peersLimit, s"Too many peers. $length exceeds limit $peersLimit")
    val peers = (0 until length).map { _ =>
      peerSpecSerializer.parse(r)
    }
    MessagesV1.PeersSpecResponse(peers)
  }
}
