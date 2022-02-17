package co.topl.network.codecs.legacy.message

import co.topl.codecs.binary.legacy.{BifrostSerializer, Reader, Writer}
import co.topl.network.codecs.legacy.peer.PeerMetadataSerializer
import co.topl.network.message.Messages.MessagesV1
import co.topl.network.peer.PeerFeature
import co.topl.utils.Extensions.LongOps

class PeersSerializer(featureSerializers: PeerFeature.Serializers, peersLimit: Int)
    extends BifrostSerializer[MessagesV1.PeersMetadataResponse] {

  private val peerMetadataSerializer = new PeerMetadataSerializer(featureSerializers)

  override def serialize(data: MessagesV1.PeersMetadataResponse, w: Writer): Unit = {
    w.putUInt(data.peers.size)
    data.peers.foreach(p => peerMetadataSerializer.serialize(p, w))
  }

  override def parse(r: Reader): MessagesV1.PeersMetadataResponse = {
    val length = r.getUInt().toIntExact
    require(length <= peersLimit, s"Too many peers. $length exceeds limit $peersLimit")
    val peers = (0 until length).map { _ =>
      peerMetadataSerializer.parse(r)
    }
    MessagesV1.PeersMetadataResponse(peers)
  }
}
