package co.topl.networking.p2p

import java.net.InetSocketAddress

case class ConnectedPeer(remoteAddress: InetSocketAddress, coordinate: (Double, Double))

case class DisconnectedPeer(remoteAddress: InetSocketAddress, coordinate: (Double, Double))

case class LocalPeer(localAddress: InetSocketAddress, coordinate: (Double, Double))
