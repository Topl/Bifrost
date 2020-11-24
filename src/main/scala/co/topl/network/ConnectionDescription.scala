package co.topl.network

import java.net.InetSocketAddress

import akka.actor.ActorRef
import co.topl.network.peer.PeerFeature

/** Case class for connection info */
case class ConnectionDescription(
  connection:       ActorRef,
  connectionId:     ConnectionId,
  ownSocketAddress: Option[InetSocketAddress],
  localFeatures:    Seq[PeerFeature]
)
