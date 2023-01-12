package co.topl.networking.p2p

import fs2.concurrent.Topic

import java.net.InetSocketAddress

/**
 * Captures the notion of serving peers in a decentralized network
 */
trait P2PServer[F[_], Client] {
  def peerChanges: F[Topic[F, PeerConnectionChange[Client]]]
  def localAddress: F[InetSocketAddress]

}

sealed abstract class PeerConnectionChange[+Client]

object PeerConnectionChanges {
  case class InboundConnectionInitializing(remoteAddress: InetSocketAddress) extends PeerConnectionChange[Nothing]
  case class OutboundConnectionInitializing(remoteAddress: InetSocketAddress) extends PeerConnectionChange[Nothing]

  case class ConnectionEstablished[Client](connectedPeer: ConnectedPeer, client: Client)
      extends PeerConnectionChange[Client]

  case class ConnectionClosed(connectedPeer: ConnectedPeer, reason: Option[Throwable])
      extends PeerConnectionChange[Nothing]
}
