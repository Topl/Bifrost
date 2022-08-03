package co.topl.network.codecs.scodecs.network.peer

import co.topl.codecs.binary.CodecCompatabilityBehavior
import co.topl.network.catsinstances.implicits._
import co.topl.network.codecs.legacy.peer.{LocalAddressPeerFeatureSerializer, PeerMetadataSerializer}
import co.topl.network.codecs.scodecs.Generators
import co.topl.network.peer.LocalAddressPeerFeature

class PeerMetadataCodecSpec extends CodecCompatabilityBehavior {

  codecCompatabilityBehavior(
    "peer metadata",
    peerMetadataCodec,
    new PeerMetadataSerializer(Map(LocalAddressPeerFeature.featureId -> LocalAddressPeerFeatureSerializer)),
    Generators.peerMetadataGen
  )
}
