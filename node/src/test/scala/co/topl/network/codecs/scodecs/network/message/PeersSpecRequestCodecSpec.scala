package co.topl.network.codecs.scodecs.network.message

import co.topl.codecs.binary.CodecCompatabilityBehavior
import co.topl.network.message.Messages.MessagesV1.PeersSpecRequest
import co.topl.network.catsinstances.implicits._
import co.topl.network.codecs.legacy.message.GetPeersSerializer
import org.scalacheck.Gen

class PeersSpecRequestCodecSpec extends CodecCompatabilityBehavior {

  codecCompatabilityBehavior(
    "peers spec request",
    peersSpecRequestCodec,
    new GetPeersSerializer(),
    Gen.const(PeersSpecRequest())
  )
}
