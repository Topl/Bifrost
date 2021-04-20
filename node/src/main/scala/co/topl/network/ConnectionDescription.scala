package co.topl.network

import akka.actor.ActorRef
import co.topl.network.peer.PeerFeature

import java.net.InetSocketAddress

/** Case class for connection info */
case class ConnectionDescription(
  connection:       ActorRef,
  connectionId:     ConnectionId,
  ownSocketAddress: Option[InetSocketAddress],
  localFeatures:    Seq[PeerFeature]
)
