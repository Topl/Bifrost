package co.topl.networking.p2p

import cats.Show

case class ConnectedPeer(remoteAddress: RemoteAddress, coordinate: (Double, Double))

case class DisconnectedPeer(remoteAddress: RemoteAddress, coordinate: (Double, Double))

case class LocalPeer(localAddress: RemoteAddress, coordinate: (Double, Double))

case class RemoteAddress(host: String, port: Int)

object RemoteAddress {
  implicit val showRemoteAddress: Show[RemoteAddress] = a => s"${a.host}:${a.port}"
}
