package bifrost.settings

import java.net.InetSocketAddress

import bifrost.network.message.MessageSpec
import bifrost.network.peer.PeerFeature
import bifrost.network.upnp.Gateway
import bifrost.utils.TimeProvider

case class BifrostContext(messageSpecs: Seq[MessageSpec[_]],
                          features: Seq[PeerFeature],
                          upnpGateway: Option[Gateway],
                          timeProvider: TimeProvider,
                          externalNodeAddress: Option[InetSocketAddress])
