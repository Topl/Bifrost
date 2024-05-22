package co.topl.networking.fsnetwork

import co.topl.models.p2p.HostId

import scala.util.Try
import inet.ipaddr.IPAddressString

class PeerFilter(ipFilters: Seq[String], idFilters: Seq[HostId]) {
  private val filterAddresses: Seq[IPAddressString] = ipFilters.flatMap(s => Try(new IPAddressString(s)).toOption)

  def remotePeerIsAcceptable(peer: RemotePeer): Boolean =
    !(idFiltered(peer) || addressFiltered(peer))

  private def idFiltered(peer: RemotePeer): Boolean =
    idFilters.contains(peer.peerId)

  private def addressFiltered(peer: RemotePeer): Boolean =
    Try(new IPAddressString(peer.address.host))
      .map(ipAddress => filterAddresses.exists(_.contains(ipAddress)))
      .fold(_ => true, identity)
}
