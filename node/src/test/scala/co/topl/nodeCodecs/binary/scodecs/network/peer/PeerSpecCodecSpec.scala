package co.topl.nodeCodecs.binary.scodecs.network.peer

import co.topl.codecs.binary.CodecCompatabilityBehavior
import co.topl.network.peer.LocalAddressPeerFeature
import co.topl.nodeCatsInstances._
import co.topl.nodeCodecs.binary.legacy.network.peer.{LocalAddressPeerFeatureSerializer, PeerSpecSerializer}
import co.topl.nodeCodecs.binary.scodecs.Generators

class PeerSpecCodecSpec extends CodecCompatabilityBehavior {

  codecCompatabilityBehavior(
    "peer spec",
    peerSpecCodec,
    new PeerSpecSerializer(Map(LocalAddressPeerFeature.featureId -> LocalAddressPeerFeatureSerializer)),
    Generators.peerSpecGen
  )
}
