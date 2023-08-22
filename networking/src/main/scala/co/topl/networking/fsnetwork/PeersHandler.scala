package co.topl.networking.fsnetwork

import co.topl.networking.fsnetwork.PeerActor.PeerActor
import co.topl.networking.fsnetwork.PeersManager.Peer
import co.topl.networking.p2p.RemoteAddress
import org.typelevel.log4cats.Logger

case class PeersHandler[F[_]: Logger](peers: Map[HostId, Peer[F]]) {
  def hosts: Set[HostId] = peers.keySet

  def get(hostId: HostId): Option[Peer[F]] = peers.get(hostId)

  def apply(hostId: HostId): Peer[F] = peers(hostId)

  def values: Iterable[Peer[F]] = peers.values

  private def getPeers(peerState: PeerState): Map[HostId, Peer[F]] =
    peers.filter { case (_: String, peer) => peer.state == peerState }

  def getHotPeers: Map[HostId, Peer[F]] = getPeers(PeerState.Hot)

  def getWarmPeers: Map[HostId, Peer[F]] = getPeers(PeerState.Warm)

  def getPreWarmPeers: Map[HostId, Peer[F]] = getPeers(PeerState.PreWarm)

  def getAvailableToConnectAddresses: Set[RemoteAddress] =
    peers
      .filterNot(_._2.state == PeerState.Banned)
      .collect { case (hostId, Peer(_, _, Some(serverPort))) =>
        RemoteAddress(hostId, serverPort)
      }
      .toSet

  def getPeersWithServerPort(state: PeerState): Set[RemoteAddress] =
    peers.collect { case (host, Peer(`state`, _, Some(address))) =>
      RemoteAddress(host, address)
    }.toSet

  def copyWithUpdatedState(toUpdate: Seq[(HostId, PeerState)]): PeersHandler[F] = {
    val updated =
      toUpdate.flatMap { case (host, newState) => peers.get(host).map(peer => host -> peer.copy(state = newState)) }
    PeersHandler(peers ++ updated)
  }

  def copyWithUpdatedState(hostId: HostId, state: PeerState): PeersHandler[F] =
    peers
      .get(hostId)
      .map(peer => PeersHandler(peers + (hostId -> peer.copy(state = state))))
      .getOrElse(this)

  def copyWithUpdatedServerPort(hostId: HostId, serverPort: Int): PeersHandler[F] =
    peers
      .get(hostId)
      .map(peer => PeersHandler(peers + (hostId -> peer.copy(serverPort = Option(serverPort)))))
      .getOrElse(this)

  def copyWithNewPeerActor(hostId: HostId, peerActor: PeerActor[F]): PeersHandler[F] =
    peers
      .get(hostId)
      .map(peer => PeersHandler(peers + (hostId -> peer.copy(actorOpt = Option(peerActor)))))
      .getOrElse(this)

  def copyWithAddedWarmPeer(host: HostId): PeersHandler[F] = {
    val toUpdate = peers.get(host) match {
      case Some(peer) => host -> peer.copy(state = PeerState.Warm)
      case None       => host -> Peer(state = PeerState.Warm, None, None)
    }
    PeersHandler(peers + toUpdate)
  }

  def copyWithAddedColdPeers(newPeers: Seq[RemoteAddress]): PeersHandler[F] = {
    val knownPeers: Set[HostId] = peers.keySet
    val toAdd: Seq[RemoteAddress] = newPeers.filterNot(ra => knownPeers.contains(ra.host))
    val peersToAdd: Map[HostId, Peer[F]] =
      toAdd.map(h => h.host -> Peer(PeerState.Cold, None, Option(h.port))).toMap
    PeersHandler(peers ++ peersToAdd)
  }

  def copyWithAddedPreWarmPeers(addressesToAdd: Seq[RemoteAddress]): PeersHandler[F] = {
    val toUpdate = addressesToAdd.map { case RemoteAddress(host, port) =>
      peers.get(host) match {
        case Some(peer @ Peer(PeerState.Cold, _, _)) =>
          host -> peer.copy(state = PeerState.PreWarm, serverPort = Option(port))
        case Some(peer) => host -> peer
        case None       => host -> Peer(PeerState.PreWarm, None, Option(port))
      }
    }.toMap
    PeersHandler(peers ++ toUpdate)
  }
}
