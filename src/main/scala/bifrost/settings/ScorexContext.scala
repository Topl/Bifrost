package bifrost.settings

import java.net.InetSocketAddress

import bifrost.network.{PeerFeature, UPnPGateway}
import bifrost.network.message.MessageSpec
import bifrost.utils.TimeProvider

case class Context(messageSpecs: Seq[MessageSpec[_]],
                         features: Seq[PeerFeature],
                         upnpGateway: Option[UPnPGateway],
                         timeProvider: TimeProvider,
                         externalNodeAddress: Option[InetSocketAddress])
