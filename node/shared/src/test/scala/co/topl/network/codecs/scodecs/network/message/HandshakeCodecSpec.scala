package co.topl.network.codecs.scodecs.network.message

import co.topl.codecs.binary.CodecCompatabilityBehavior
import co.topl.network.codecs.scodecs.Generators
import co.topl.network.catsinstances.implicits._
import co.topl.network.codecs.legacy.message.HandshakeSerializer

class HandshakeCodecSpec extends CodecCompatabilityBehavior {

  codecCompatabilityBehavior(
    "handshake message",
    handshakeCodec,
    new HandshakeSerializer(Map(), 10000),
    Generators.handshakeGen
  )
}
