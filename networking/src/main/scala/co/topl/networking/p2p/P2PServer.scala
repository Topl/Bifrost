package co.topl.networking.p2p

import fs2.concurrent.Topic

/**
 * Captures the notion of serving peers in a decentralized network
 */
trait P2PServer[F[_]] {
  def peerChanges: F[Topic[F, PeerConnectionChange]]
  def localAddress: F[RemoteAddress]

}

sealed abstract class PeerConnectionChange

object PeerConnectionChanges {

  case class InboundConnectionInitializing(remoteAddress: RemoteAddress, localAddress: RemoteAddress)
      extends PeerConnectionChange

  case class OutboundConnectionInitializing(remoteAddress: RemoteAddress) extends PeerConnectionChange

  case class ChangedRemotePeer(oldPeer: DisconnectedPeer, newPeer: DisconnectedPeer) extends PeerConnectionChange

  case class ConnectionEstablished(connectedPeer: ConnectedPeer, localAddress: RemoteAddress)
      extends PeerConnectionChange

  case class ConnectionClosed(disconnectedPeer: DisconnectedPeer, reason: Option[Throwable])
      extends PeerConnectionChange

  case class RemotePeerApplicationLevel(connectedPeer: ConnectedPeer, applicationLevelEnabled: Boolean)
      extends PeerConnectionChange
}
