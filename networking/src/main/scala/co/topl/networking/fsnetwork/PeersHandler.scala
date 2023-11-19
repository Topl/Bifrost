package co.topl.networking.fsnetwork

import cats.Applicative
import cats.data.Chain
import cats.effect.Async
import cats.implicits._
import co.topl.networking.fsnetwork.PeerActor.PeerActor
import co.topl.networking.fsnetwork.PeersHandler.allowedTransition
import co.topl.networking.p2p.RemoteAddress
import org.typelevel.log4cats.Logger

object PeersHandler {

  def allowedTransition(initialState: PeerState): Set[PeerState] =
    initialState match {
      case PeerState.Unknown => Set(PeerState.Cold)
      case PeerState.Banned  => Set.empty
      // Cold to Cold -- special case for closing connection if peers are already in the cold state
      case PeerState.Cold => Set(PeerState.Cold, PeerState.Banned, PeerState.Warm)
      // Warm to Warm -- special case for opening new connection case, will be removed later
      case PeerState.Warm => Set(PeerState.Cold, PeerState.Warm, PeerState.Hot, PeerState.Banned)
      case PeerState.Hot  => Set(PeerState.Cold, PeerState.Banned)
    }
}

case class PeersHandler[F[_]: Async: Logger](
  peers:         Map[HostId, Peer[F]],
  networkConfig: P2PNetworkConfig
) {

  private def couldTransit(hostId: HostId, newState: PeerState): Boolean = {
    val currentState: PeerState = peers.get(hostId).map(_.state).getOrElse(PeerState.Unknown)
    allowedTransition(currentState).contains(newState)
  }

  def get(hostId: HostId): Option[Peer[F]] = peers.get(hostId)

  def apply(hostId: HostId): Peer[F] = peers(hostId)

  def forPeersWithActor[R](f: Peer[F] => R): Seq[R] =
    peers.values.filter(_.actorOpt.isDefined).map(f).toSeq

  def haveNoActorForHost(hostId: HostId): Boolean = peers.get(hostId).flatMap(_.actorOpt).isEmpty

  private def hostIsBanned(hostId: HostId): Boolean = peers.get(hostId).exists(_.state == PeerState.Banned)

  def hostIsNotBanned(hostId: HostId): Boolean = !hostIsBanned(hostId)

  private def getPeers(peerState: PeerState): Map[HostId, Peer[F]] =
    peers.filter { case (_: String, peer) => peer.state == peerState }

  def getHotPeers: Map[HostId, Peer[F]] = getPeers(PeerState.Hot)

  def getWarmPeers: Map[HostId, Peer[F]] = getPeers(PeerState.Warm)

  def getWarmPeersWithActor: Map[HostId, Peer[F]] = getPeers(PeerState.Warm).filter(_._2.actorOpt.isDefined)

  def getColdPeers: Map[HostId, Peer[F]] = getPeers(PeerState.Cold)

  def getRemotePeers: Set[RemotePeer] =
    peers
      .filterNot(_._2.state == PeerState.Banned)
      .collect { case (hostId, Peer(_, _, _, Some(serverPort), _, _, blockProvidingRep, performanceRep, _)) =>
        RemotePeer(RemoteAddress(hostId, serverPort), blockProvidingRep, performanceRep)
      }
      .toSet

  def moveToState(
    forUpdate:                Set[HostId],
    newState:                 PeerState,
    peerActorCloseAndRelease: Peer[F] => F[Unit]
  ): F[PeersHandler[F]] = {
    val updateF: F[Seq[(HostId, Peer[F])]] =
      forUpdate
        .filter(host => peers.contains(host) && couldTransit(host, newState))
        .map(host => (host, peers(host)))
        .toSeq
        .traverse { case (host, oldPeer) =>
          for {
            peerWithClosedTimestamp <- updateCloseTimestamps(oldPeer, newState).copy(state = newState).pure[F]
            _                       <- updateStateIfChanged(oldPeer, peerWithClosedTimestamp)
            peerWithRep             <- updateReputation(oldPeer, peerWithClosedTimestamp).pure[F]
            actorOpt                <- closePeerIfNecessary(peerWithClosedTimestamp, peerActorCloseAndRelease)
            newPeer                 <- peerWithRep.copy(actorOpt = actorOpt).pure[F]
            _                       <- Logger[F].info(show"Move $host from ${oldPeer.state} to ${newPeer.state}")
          } yield host -> newPeer
        }

    updateF.map(update => this.copy(peers = peers ++ update))
  }

  private def updateCloseTimestamps(peerToUpdate: Peer[F], newState: PeerState): Peer[F] =
    if (peerToUpdate.state.networkLevel && !newState.networkLevel) {
      val closeTimestamp = System.currentTimeMillis()
      val timeStampWindow = networkConfig.networkProperties.closeTimeoutWindowInMs
      val eligibleTimestamps = peerToUpdate.closedTimestamps.filter(_ >= (closeTimestamp - timeStampWindow))
      val newPeer = peerToUpdate.copy(closedTimestamps = eligibleTimestamps :+ closeTimestamp)
      newPeer
    } else {
      peerToUpdate
    }

  private def updateStateIfChanged(oldPeer: Peer[F], newPeer: Peer[F]): F[Unit] =
    if (
      (oldPeer.state.applicationLevel != newPeer.state.applicationLevel) ||
      (oldPeer.state.networkLevel != newPeer.state.networkLevel)
    ) {
      newPeer.sendNoWait(PeerActor.Message.UpdateState(newPeer.state.networkLevel, newPeer.state.applicationLevel))
    } else {
      Applicative[F].unit
    }

  private def updateReputation(oldPeer: Peer[F], newPeer: Peer[F]): Peer[F] =
    if (oldPeer.state.isActive && !newPeer.state.isActive) {
      newPeer.copy(perfRep = 0.0, blockRep = 0.0, newRep = 0)
    } else if (!oldPeer.state.applicationLevel && newPeer.state.applicationLevel) {
      newPeer.copy(newRep = networkConfig.remotePeerNoveltyInSlots)
    } else { newPeer }

  private def closePeerIfNecessary(peer: Peer[F], peerActorRelease: Peer[F] => F[Unit]): F[Option[PeerActor[F]]] =
    if ((!peer.remoteNetworkLevel && !peer.state.networkLevel) || peer.state == PeerState.Banned) {
      peerActorRelease(peer) >>
      Option.empty[PeerActor[F]].pure[F]
    } else {
      peer.actorOpt.pure[F]
    }

  def copyWithNewHost(host: HostId): PeersHandler[F] = {
    val peerToAdd =
      peers.get(host) match {
        case None => host -> Peer(PeerState.Cold, None, host, None, Seq.empty, remoteNetworkLevel = false, 0.0, 0.0, 0)
        case Some(peer) => host -> peer
      }
    this.copy(peers = peers + peerToAdd)
  }

  def copyWithNewPeerActor(hostId: HostId, peerActor: PeerActor[F]): PeersHandler[F] =
    peers
      .get(hostId)
      .map { peer =>
        val hostAndPeer = hostId -> peer.copy(actorOpt = Option(peerActor))
        this.copy(peers = peers + hostAndPeer)
      }
      .getOrElse(this)

  def copyWithAddedPeers(newPeers: Chain[RemotePeer]): PeersHandler[F] = {
    val peersToAdd: Map[HostId, Peer[F]] =
      newPeers.toList.map { case RemotePeer(RemoteAddress(host, port), initialBlockReputation, initialPerfReputation) =>
        peers.get(host) match {
          case None =>
            host -> Peer(
              PeerState.Cold,
              None,
              host,
              port.some,
              Seq.empty,
              remoteNetworkLevel = false,
              initialBlockReputation,
              initialPerfReputation,
              0
            )
          case Some(peer) => host -> peer.copy(remoteServerPort = port.some)
        }
      }.toMap

    this.copy(peers = peers ++ peersToAdd)
  }

  private def getAffectedPeers(peersToChange: Set[HostId]): Set[(HostId, Peer[F])] =
    peersToChange.flatMap(id => peers.get(id).map(peer => id -> peer))

  def copyWithUpdatedReputation(
    perfRepMap:    Map[HostId, HostReputationValue] = Map.empty,
    blockRepMap:   Map[HostId, HostReputationValue] = Map.empty,
    noveltyRepMap: Map[HostId, Long] = Map.empty
  ): PeersHandler[F] = {
    val toUpdate = perfRepMap.keySet ++ blockRepMap.keySet ++ noveltyRepMap.keySet
    val newPeers =
      getAffectedPeers(toUpdate).map { case (id, peer) =>
        val perfRep = perfRepMap.getOrElse(id, peer.perfRep)
        val blockRep = blockRepMap.getOrElse(id, peer.blockRep)
        val newRep = noveltyRepMap.getOrElse(id, peer.newRep)
        id -> peer.copy(perfRep = perfRep, blockRep = blockRep, newRep = newRep)
      }.toMap

    this.copy(peers = peers ++ newPeers)
  }

  def copyWithUpdatedServerPort(hostId: HostId, serverPortOpt: Option[Int]): PeersHandler[F] =
    peers
      .get(hostId)
      .map(peer => this.copy(peers = peers + (hostId -> peer.copy(remoteServerPort = serverPortOpt))))
      .getOrElse(this)

  def copyWithUpdatedNetworkLevel(
    hostIds:          Set[HostId],
    netLevel:         Boolean,
    peerActorRelease: Peer[F] => F[Unit]
  ): F[PeersHandler[F]] = {
    val updatedPeersF =
      getAffectedPeers(hostIds).toSeq.traverse { case (hostId, peer) =>
        val peerWithNetworkLevel = peer.copy(remoteNetworkLevel = netLevel)

        closePeerIfNecessary(peerWithNetworkLevel, peerActorRelease).map { actorOpt =>
          (hostId, peerWithNetworkLevel.copy(actorOpt = actorOpt))
        }
      }

    updatedPeersF.map(peers => this.copy(peers = this.peers ++ peers.toMap))
  }

  def copyWithRemovedColdPeersWithoutActor(toRemove: Set[HostId]): PeersHandler[F] = {
    val filteredToRemove =
      getColdPeers.view.filterKeys(toRemove).filter(_._2.actorOpt.isEmpty).keySet
    val newPeers = peers -- filteredToRemove
    this.copy(peers = newPeers)
  }
}

case class Peer[F[_]: Logger](
  state:              PeerState,
  actorOpt:           Option[PeerActor[F]],
  ip:                 String,
  remoteServerPort:   Option[Int],
  closedTimestamps:   Seq[Long],
  remoteNetworkLevel: Boolean,
  blockRep:           HostReputationValue,
  perfRep:            HostReputationValue,
  newRep:             Long
) {
  def haveNoConnection: Boolean = actorOpt.isEmpty

  def sendNoWait(message: PeerActor.Message): F[Unit] =
    actorOpt match {
      case Some(actor) => actor.sendNoWait(message)
      case None        => Logger[F].trace(show"Send message to peer with no running client")
    }

  def asRemoteAddress: Option[RemoteAddress] = remoteServerPort.map(RemoteAddress(ip, _))

  val reputation: Double = (blockRep + perfRep) / 2
}
