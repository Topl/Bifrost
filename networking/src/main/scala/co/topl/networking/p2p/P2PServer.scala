package co.topl.networking.p2p

import java.net.InetSocketAddress

trait P2PServer[F[_]] {
  def stop(): F[Unit]
  def connectedPeers(): F[Set[ConnectedPeer]]
  def localAddress: F[InetSocketAddress]
}
