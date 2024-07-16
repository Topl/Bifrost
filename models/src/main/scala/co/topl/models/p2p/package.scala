package co.topl.models

import cats.Show
import cats.implicits._

package object p2p {

  case class RemoteAddress(host: String, port: Int) {
    override def toString: String = show"$host:$port"
  }

  object RemoteAddress {
    implicit val showRemoteAddress: Show[RemoteAddress] = a => s"${a.host}:${a.port}"
  }
  case class HostId(id: Bytes) extends AnyVal

  type HostReputationValue =
    Double // will be more complex, to get high reputation host shall fulfill different criteria

  sealed trait NetworkCommands

  object NetworkCommands {
    case class ForgetPeer(hostId: HostId) extends NetworkCommands
    case class AddPeer(remoteAddress: RemoteAddress, remotepeerIdOpt: Option[HostId]) extends NetworkCommands
  }

}
