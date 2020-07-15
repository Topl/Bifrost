package bifrost.settings

import java.net.InetSocketAddress

import bifrost.network.PeerFeature
import bifrost.network.message.MessageSpec
import bifrost.network.upnp.UPnPGateway
import bifrost.utils.TimeProvider

case class BifrostContext(messageSpecs: Seq[MessageSpec[_]],
                          features: Seq[PeerFeature],
                          upnpGateway: Option[UPnPGateway],
                          timeProvider: TimeProvider,
                          externalNodeAddress: Option[InetSocketAddress])
