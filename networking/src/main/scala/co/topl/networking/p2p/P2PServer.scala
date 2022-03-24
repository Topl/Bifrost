package co.topl.networking.p2p

import akka.NotUsed
import akka.stream.scaladsl.Source

import java.net.InetSocketAddress

/**
 * Captures the notion of serving peers in a decentralized network
 */
trait P2PServer[F[_], Client] {
  def stop(): F[Unit]
  def newConnectedPeers: F[Source[(ConnectedPeer, Client), NotUsed]]
  def connectedPeers(): F[Map[ConnectedPeer, Client]]
  def localAddress: F[InetSocketAddress]
}
