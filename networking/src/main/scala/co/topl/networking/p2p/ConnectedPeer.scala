package co.topl.networking.p2p

import java.net.InetSocketAddress

case class ConnectedPeer(remoteAddress: InetSocketAddress)

case class LocalPeer(localAddress: InetSocketAddress)
