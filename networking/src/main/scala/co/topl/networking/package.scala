package co.topl

import cats.Monad
import cats.data.OptionT
import cats.implicits._
import co.topl.models.p2p._
import co.topl.networking.fsnetwork.RemotePeer
import co.topl.node.models.KnownHost
import com.comcast.ip4s.{IpAddress, SocketAddress}
import fs2.Chunk
import fs2.io.net.Socket

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

  implicit class SocketOps[F[_]](val socket: Socket[F]) extends AnyVal {

    /**
     * Calling Socket.read(numberOfBytes) may not return all `numberOfBytes`. This situation needs to be checked,
     * and additional bytes should be requested until the expected number of bytes are received.
     * @param length the total number of bytes desired
     * @return a chunk of bytes with the desired length, or None if the socket is closed
     */
    def readExactly(length: Int)(implicit monadF: Monad[F]): F[Option[Chunk[Byte]]] =
      (length, Chunk.empty[Byte]).tailRecM { case (remaining, acc) =>
        OptionT(socket.read(remaining))
          .fold(none[Chunk[Byte]].asRight[(Int, Chunk[Byte])])(bytes =>
            if (bytes.size < remaining) Left((remaining - bytes.size, acc ++ bytes))
            else Right(Some(acc ++ bytes))
          )
      }

  }
}
