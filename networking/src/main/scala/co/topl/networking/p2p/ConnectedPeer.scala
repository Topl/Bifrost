package co.topl.networking.p2p

import java.net.InetSocketAddress
import scala.concurrent.duration.FiniteDuration

case class ConnectedPeer(remoteAddress: InetSocketAddress, coordinate: (Double, Double))

case class DisconnectedPeer(remoteAddress: InetSocketAddress, coordinate: (Double, Double))

case class LocalPeer(localAddress: InetSocketAddress, coordinate: (Double, Double))
