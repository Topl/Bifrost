package co.topl.network.codecs.scodecs.network.message

import co.topl.codecs.binary.CodecCompatabilityBehavior
import co.topl.network.message.Messages.MessagesV1.PeersMetadataRequest
import co.topl.network.catsinstances.implicits._
import co.topl.network.codecs.legacy.message.PeersMetadataRequestSerializer
import org.scalacheck.Gen

class PeersMetadataRequestCodecSpec extends CodecCompatabilityBehavior {

  codecCompatabilityBehavior(
    "peers spec request",
    peersMetadataRequestCodec,
    new PeersMetadataRequestSerializer(),
    Gen.const(PeersMetadataRequest())
  )
}
