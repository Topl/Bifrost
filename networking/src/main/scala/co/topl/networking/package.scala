package co.topl

import co.topl.networking.p2p.RemoteAddress
import co.topl.node.models.KnownHost
import com.comcast.ip4s.{IpAddress, SocketAddress}

import java.net.InetAddress
import java.nio.ByteBuffer

package object networking {

  /**
   * Encodes an integer into 4 bytes using Big-Endian format
   */
  def encodeInt(value: Int): Array[Byte] =
    ByteBuffer.allocate(4).putInt(value).array()

  implicit class KnownHostOps(knownHost: KnownHost) {
    def asRemoteAddress: RemoteAddress = RemoteAddress(knownHost.host, knownHost.port)
  }

  implicit class RemoteAddressOps(remoteAddress: RemoteAddress) {
    def asKnownHost: KnownHost = KnownHost(remoteAddress.host, remoteAddress.port)

    def isSpecialHost: Boolean = {
      val ip = InetAddress.getByName(remoteAddress.host)
      ip.isLoopbackAddress || ip.isMCGlobal || ip.isMCLinkLocal || ip.isAnyLocalAddress || ip.isMulticastAddress ||
      ip.getHostName == "255.255.255.255"
    }
  }

  implicit class SocketAddressOps(address: SocketAddress[IpAddress]) {
    def asRemoteAddress: RemoteAddress = RemoteAddress(address.host.toUriString, address.port.value)
  }
}
