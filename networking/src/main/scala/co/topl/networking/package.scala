package co.topl

import co.topl.models.p2p._
import co.topl.networking.fsnetwork.RemotePeer
import co.topl.node.models.KnownHost
import com.comcast.ip4s.{IpAddress, SocketAddress}

import java.net.InetAddress
import java.nio.ByteBuffer
import scala.util.Try

package object networking {

  /**
   * Encodes an integer into 4 bytes using Big-Endian format
   */
  def encodeInt(value: Int): Array[Byte] =
    ByteBuffer.allocate(4).putInt(value).array()

  implicit class KnownHostOps(knownHost: KnownHost) {
    def asRemotePeer: RemotePeer = RemotePeer(HostId(knownHost.id), RemoteAddress(knownHost.host, knownHost.port))
  }

  implicit class RemoteAddressOps(remoteAddress: RemoteAddress) {

    def isSpecialHost: Boolean =
      Try {
        val ip = InetAddress.getByName(remoteAddress.host)
        ip.isLoopbackAddress || ip.isMCGlobal || ip.isMCLinkLocal || ip.isAnyLocalAddress || ip.isMulticastAddress ||
        ip.getHostName == "255.255.255.255"
      }.getOrElse(true)
  }

  implicit class SocketAddressOps(address: SocketAddress[IpAddress]) {
    def asRemoteAddress: RemoteAddress = RemoteAddress(address.host.toUriString, address.port.value)
  }
}
