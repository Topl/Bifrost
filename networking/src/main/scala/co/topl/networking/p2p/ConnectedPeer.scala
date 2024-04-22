package co.topl.networking.p2p

import cats.Show
import cats.implicits._
import co.topl.models.Bytes
import co.topl.typeclasses.implicits._

case class ConnectedPeer(remoteAddress: RemoteAddress, p2pVK: Bytes, networkVersion: Bytes) {
  override def toString: String = show"ConnectedPeer(address=$remoteAddress, id=$p2pVK, version=$networkVersion)"
}

case class DisconnectedPeer(remoteAddress: RemoteAddress, p2pVK: Option[Bytes]) {
  override def toString: String = show"DisconnectedPeer(address=$remoteAddress, id=${p2pVK.fold("?")(_.show)})"
}

case class LocalPeer(localAddress: RemoteAddress, p2pVK: Bytes, p2pSK: Bytes) {
  override def toString: String = show"LocalPeer(address=$localAddress, id=$p2pVK)"
}

case class RemoteAddress(host: String, port: Int) {

  override def toString: String = show"$host:$port"
}

object RemoteAddress {
  implicit val showRemoteAddress: Show[RemoteAddress] = a => s"${a.host}:${a.port}"
}
