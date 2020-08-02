package bifrost.settings

import java.net.InetSocketAddress

import bifrost.network.message._
import bifrost.network.upnp.Gateway
import bifrost.network.{NodeViewSynchronizer, peer}
import bifrost.utils.NetworkTimeProvider

import scala.concurrent.ExecutionContext

class BifrostContext(settings: AppSettings, val upnpGateway: Option[Gateway]) (implicit ec: ExecutionContext) {

  // save your address for sending to others peers
  val externalNodeAddress: Option[InetSocketAddress] = {
    settings.network.declaredAddress orElse {
      upnpGateway.map(u => new InetSocketAddress(u.externalAddress, u.mappedPort))
    }
  }

  // save a common time provider to be used
  val timeProvider = new NetworkTimeProvider(settings.ntp)

  // enumerate features and message specs present for communicating between peers
  val features: Seq[peer.PeerFeature] = Seq()
  val featureSerializers: peer.PeerFeature.Serializers = features.map(f => f.featureId -> f.serializer).toMap

  // instantiate and populate the local message handler for peer management requests from remote peers
  val peerSyncRemoteMessages: peer.PeerSynchronizer.RemoteMessageHandler = {
    val peersSpec = new PeersSpec(featureSerializers, settings.network.maxPeerSpecObjects)
    val getPeersSpec = new GetPeersSpec

    peer.PeerSynchronizer.RemoteMessageHandler(peersSpec, getPeersSpec)
  }

  // instantiate and populate the local message handler for node view management requests from remote peers
  val nodeViewSyncRemoteMessages: NodeViewSynchronizer.RemoteMessageHandler = {
    val syncInfoSpec = BifrostSyncInfoMessageSpec
    val invSpec = new InvSpec(settings.network.maxInvObjects)
    val requestModifierSpec = new RequestModifierSpec(settings.network.maxInvObjects)
    val modifiersSpec = new ModifiersSpec(settings.network.maxPacketSize)

    NodeViewSynchronizer.RemoteMessageHandler(syncInfoSpec, invSpec, requestModifierSpec, modifiersSpec)
  }

  val messageSpecs: Seq[MessageSpec[_]] = peerSyncRemoteMessages.toSeq ++ nodeViewSyncRemoteMessages.toSeq

}
