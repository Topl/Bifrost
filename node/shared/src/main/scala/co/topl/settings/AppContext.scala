package co.topl.settings

import co.topl.network.peer
import co.topl.network.utils.UPnPGateway
import co.topl.utils.NetworkType

import java.net.InetSocketAddress

/**
 * Info that Bifrost needs based on the settings and user options
 *
 * @param settings application settings
 * @param startupOpts user defined startup options
 * @param upnpGateway Option of Gateway class handling gateway device and port forwarding
 */
class AppContext(
  val settings:    AppSettings,
  startupOpts:     StartupOpts,
  val upnpGateway: Option[UPnPGateway]
) {

  /** Save your address for sending to others peers */
  val externalNodeAddress: Option[InetSocketAddress] =
    settings.network.declaredAddress orElse {
      upnpGateway.map(u => new InetSocketAddress(u.externalAddress, u.mappedPort))
    }

  /** Save chosen network for loading genesis config */
  val networkType: NetworkType =
    startupOpts.networkTypeOpt match {
      case Some(network) => network
      case None          => NetworkType.PrivateTestnet
    }

  /** Enumerate features and message specs present for communicating between peers */
  val features: Seq[peer.PeerFeature] = Seq()
}
