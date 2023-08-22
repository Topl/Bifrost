package co.topl.networking.fsnetwork

import co.topl.networking.fsnetwork.PeerActor.PeerActor
import co.topl.networking.fsnetwork.PeersHandler.allowedTransition
import co.topl.networking.fsnetwork.PeersManager.Peer
import co.topl.networking.p2p.RemoteAddress
import org.typelevel.log4cats.Logger

object PeersHandler {

  def allowedTransition(initialState: PeerState): Set[PeerState] =
    initialState match {
      case PeerState.Unknown => Set(PeerState.Cold)
      case PeerState.Banned  => Set.empty
      case PeerState.Cold    => Set(PeerState.PreWarm, PeerState.Warm)
      case PeerState.PreWarm => Set(PeerState.Cold, PeerState.Warm)
      case PeerState.Warm    => Set(PeerState.Cold, PeerState.Hot, PeerState.Banned)
      case PeerState.Hot     => Set(PeerState.Cold, PeerState.Banned)
    }
}

case class PeersHandler[F[_]: Logger](peers: Map[HostId, Peer[F]]) {

  private def couldTransit(hostId: HostId, newState: PeerState): Boolean = {
    val currentState: PeerState = peers.get(hostId).map(_.state).getOrElse(PeerState.Unknown)
    if (currentState == newState) {
      true
    } else {
      allowedTransition(currentState).contains(newState)
    }
  }

  def get(hostId: HostId): Option[Peer[F]] = peers.get(hostId)

  def apply(hostId: HostId): Peer[F] = peers(hostId)

  def values: Iterable[Peer[F]] = peers.values

  private def getPeers(peerState: PeerState): Map[HostId, Peer[F]] =
    peers.filter { case (_: String, peer) => peer.state == peerState }

  def getHotPeers: Map[HostId, Peer[F]] = getPeers(PeerState.Hot)

  def getWarmPeers: Map[HostId, Peer[F]] = getPeers(PeerState.Warm)

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

  def moveToState(forUpdate: HostId, newState: PeerState): (Map[HostId, Peer[F]], PeersHandler[F]) =
    moveToState(Set(forUpdate), newState)

  def moveToState(forUpdate: Set[HostId], newState: PeerState): (Map[HostId, Peer[F]], PeersHandler[F]) = {
    val update: Map[HostId, Peer[F]] = forUpdate
      .filter(host => couldTransit(host, newState) && peers.contains(host))
      .map(host => host -> peers(host).copy(state = newState))
      .toMap
    (update, PeersHandler(peers ++ update))
  }

  def copyWithAddedRemoteAddresses(newPeers: Set[RemoteAddress]): PeersHandler[F] =
    copyWithAddedHostAndPort(newPeers.map { case RemoteAddress(host, port) => (host, Option(port)) })

  def copyWithAddedHost(host: HostId): PeersHandler[F] =
    copyWithAddedHostAndPort(Set(host -> None))

  private def copyWithAddedHostAndPort(newPeers: Set[(HostId, Option[Int])]): PeersHandler[F] = {
    val peersToAdd = newPeers
      .withFilter { case (host, _) => couldTransit(host, PeerState.Cold) }
      .map { case (host, port) =>
        peers.get(host) match {
          case None                          => host -> Peer(PeerState.Cold, None, port)
          case Some(peer @ Peer(_, _, None)) => host -> peer.copy(serverPort = port)
          case Some(peer)                    => host -> peer
        }
      }
      .toMap

    PeersHandler(peers ++ peersToAdd)
  }

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
}
