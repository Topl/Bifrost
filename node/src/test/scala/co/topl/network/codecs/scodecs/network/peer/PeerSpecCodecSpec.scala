package co.topl.network.codecs.scodecs.network.peer

import co.topl.codecs.binary.CodecCompatabilityBehavior
import co.topl.network.codecs.legacy.peer.PeerSpecSerializer
import co.topl.network.codecs.scodecs.Generators
import co.topl.network.peer.LocalAddressPeerFeature
import co.topl.network.catsinstances.implicits._
import co.topl.network.codecs.legacy.peer.{LocalAddressPeerFeatureSerializer, PeerSpecSerializer}

class PeerSpecCodecSpec extends CodecCompatabilityBehavior {

  codecCompatabilityBehavior(
    "peer spec",
    peerSpecCodec,
    new PeerSpecSerializer(Map(LocalAddressPeerFeature.featureId -> LocalAddressPeerFeatureSerializer)),
    Generators.peerSpecGen
  )
}
