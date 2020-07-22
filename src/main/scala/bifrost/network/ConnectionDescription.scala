package bifrost.network

import java.net.InetSocketAddress

import akka.actor.ActorRef
import bifrost.network.peer.PeerFeature

case class ConnectionDescription(connection: ActorRef,
                                 connectionId: ConnectionId,
                                 ownSocketAddress: Option[InetSocketAddress],
                                 localFeatures: Seq[PeerFeature])
