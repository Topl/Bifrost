package co.topl.nodeCodecs.binary.legacy.network.message

import co.topl.codecs.binary.legacy.{BifrostSerializer, Reader, Writer}
import co.topl.network.message.Messages.MessagesV1
import co.topl.network.peer.PeerFeature
import co.topl.nodeCodecs.binary.legacy.network.peer.PeerSpecSerializer

class HandshakeSerializer(featureSerializers: PeerFeature.Serializers, sizeLimit: Int)
    extends BifrostSerializer[MessagesV1.Handshake] {

  private val peersDataSerializer = new PeerSpecSerializer(featureSerializers)

  override def serialize(obj: MessagesV1.Handshake, w: Writer): Unit = {
    w.putULong(obj.time)
    peersDataSerializer.serialize(obj.peerSpec, w)
  }

  override def parse(r: Reader): MessagesV1.Handshake = {
    require(r.remaining <= sizeLimit, s"Too big handshake. Size ${r.remaining} exceeds $sizeLimit limit")
    val t = r.getULong()
    val data = peersDataSerializer.parse(r)
    MessagesV1.Handshake(data, t)
  }
}
