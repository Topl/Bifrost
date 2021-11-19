package co.topl.nodeCodecs.binary.scodecs.network.message

import co.topl.codecs.binary.CodecCompatabilityBehavior
import co.topl.nodeCatsInstances._
import co.topl.nodeCodecs.binary.legacy.network.message.HandshakeSerializer
import co.topl.nodeCodecs.binary.scodecs.Generators

class HandshakeCodecSpec extends CodecCompatabilityBehavior {

  codecCompatabilityBehavior(
    "handshake message",
    handshakeCodec,
    new HandshakeSerializer(Map(), 10000),
    Generators.handshakeGen
  )
}
