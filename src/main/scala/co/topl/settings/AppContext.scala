package co.topl.settings

import java.net.InetSocketAddress

import co.topl.network.message._
import co.topl.network.upnp.Gateway
import co.topl.network.{NodeViewSynchronizer, PeerSynchronizer, peer}
import co.topl.utils.NetworkTimeProvider

import scala.concurrent.ExecutionContext

class AppContext ( settings: AppSettings,
                   startupOpts: StartupOpts,
                   val upnpGateway: Option[Gateway]
                 )(implicit ec: ExecutionContext) {

  // save your address for sending to others peers
  val externalNodeAddress: Option[InetSocketAddress] = {
    settings.network.declaredAddress orElse {
      upnpGateway.map(u => new InetSocketAddress(u.externalAddress, u.mappedPort))
    }
  }

  // save a common time provider to be used
  val timeProvider = new NetworkTimeProvider(settings.ntp)

  // save chosen network for loading genesis config
  val networkType: NetworkType = {
    val opts = startupOpts.runtimeParams
    startupOpts.networkTypeOpt match {
      case Some(network) => NetworkType.fillNetworkType(network, opts)
      case None          => NetworkType.PrivateNet(opts)
    }
  }

  // enumerate features and message specs present for communicating between peers
  val features: Seq[peer.PeerFeature] = Seq()
  val featureSerializers: peer.PeerFeature.Serializers = features.map(f => f.featureId -> f.serializer).toMap

  // instantiate and populate the local message handler for peer management requests from remote peers
  val peerSyncRemoteMessages: PeerSynchronizer.RemoteMessageHandler = {
    val getPeersSpec = new GetPeersSpec
    val peersSpec = new PeersSpec(featureSerializers, settings.network.maxPeerSpecObjects)

    PeerSynchronizer.RemoteMessageHandler(peersSpec, getPeersSpec)
  }

  // instantiate and populate the local message handler for node view management requests from remote peers
  val nodeViewSyncRemoteMessages: NodeViewSynchronizer.RemoteMessageHandler = {
    val syncInfoSpec = new SyncInfoSpec
    val invSpec = new InvSpec(settings.network.maxInvObjects)
    val requestModifierSpec = new RequestModifierSpec(settings.network.maxInvObjects)
    val modifiersSpec = new ModifiersSpec(settings.network.maxPacketSize)

    NodeViewSynchronizer.RemoteMessageHandler(syncInfoSpec, invSpec, requestModifierSpec, modifiersSpec)
  }

  val messageSpecs: Seq[MessageSpec[_]] = peerSyncRemoteMessages.toSeq ++ nodeViewSyncRemoteMessages.toSeq

}
