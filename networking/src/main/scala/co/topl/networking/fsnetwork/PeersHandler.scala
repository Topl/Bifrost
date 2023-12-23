package co.topl.networking.fsnetwork

import cats.Applicative
import cats.data.Chain
import cats.effect.Async
import cats.implicits._
import co.topl.networking.fsnetwork.PeerActor.PeerActor
import co.topl.networking.fsnetwork.PeersHandler.allowedTransition
import org.typelevel.log4cats.Logger
import co.topl.networking.fsnetwork.P2PShowInstances._
import co.topl.networking.p2p.RemoteAddress

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
    peers.filter { case (_: HostId, peer) => peer.state == peerState }

  def getHotPeers: Map[HostId, Peer[F]] = getPeers(PeerState.Hot)

  def getWarmPeers: Map[HostId, Peer[F]] = getPeers(PeerState.Warm)

  def getWarmPeersWithActor: Map[HostId, Peer[F]] = getPeers(PeerState.Warm).filter(_._2.actorOpt.isDefined)

  def getColdPeers: Map[HostId, Peer[F]] = getPeers(PeerState.Cold)

  def getRemotePeers: Set[KnownRemotePeer] =
    peers
      .filterNot(_._2.state == PeerState.Banned)
      .collect { case (_, Peer(_, _, _, Some(server), _, _, blockProvidingRep, performanceRep, _)) =>
        KnownRemotePeer(server.peerId, server.address, blockProvidingRep, performanceRep)
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
      peerToUpdate.copy(closedTimestamps = peerToUpdate.closedTimestamps :+ System.currentTimeMillis())
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

  def copyWithUpdatedId(oldId: HostId, newId: HostId, peerActorRelease: Peer[F] => F[Unit]): F[PeersHandler[F]] =
    (peers.get(oldId), peers.get(newId)) match {
      case (Some(peerForOldId), None) =>
        val newPeers = (peers - oldId) + (newId -> peerForOldId)
        this.copy(peers = newPeers).pure[F]
      case (Some(peerForOldId), Some(outdatedPeer)) =>
        if (outdatedPeer.state == PeerState.Banned) {
          val newPeers = peers - oldId
          Logger[F].warn(
            show"Change id from $oldId to $newId but peer $newId already exist and banned, close peer $peerForOldId"
          ) >>
          peerActorRelease(peerForOldId) >>
          this.copy(peers = newPeers).pure[F]
        } else {
          val newPeers = (peers - oldId) + (newId -> peerForOldId)
          Logger[F].warn(
            show"Change id from $oldId to $newId but peer $newId already exist, close outdated peer $newId first"
          ) >>
          peerActorRelease(outdatedPeer) >>
          this.copy(peers = newPeers).pure[F]
        }
      case (None, _) =>
        Logger[F].error(show"Try to change id from $oldId to $newId but $oldId is not exist") >>
        Async[F].pure(this)
    }

  def copyWithUpdatedPeer(
    host:      HostId,
    address:   RemoteAddress,
    asServer:  Option[RemotePeer],
    peerActor: PeerActor[F]
  ): PeersHandler[F] = {
    val peerToAdd =
      peers.get(host) match {
        case None =>
          host -> Peer(
            PeerState.Cold,
            peerActor.some,
            address.some,
            asServer,
            Seq.empty,
            remoteNetworkLevel = false,
            0,
            0,
            0
          )
        case Some(peer) =>
          val mergedServerAddress = asServer.orElse(peer.asServer)
          host -> peer.copy(connectedAddress = address.some, asServer = mergedServerAddress, actorOpt = peerActor.some)
      }
    this.copy(peers = peers + peerToAdd)
  }

  def copyWithAddedPeers(newPeers: Chain[KnownRemotePeer]): PeersHandler[F] = {
    val peersToAdd: Map[HostId, Peer[F]] =
      newPeers.toList.map { case KnownRemotePeer(id, ra, initialBlockReputation, initialPerfReputation) =>
        peers.get(id) match {
          case None =>
            id -> Peer(
              PeerState.Cold,
              None,
              None,
              RemotePeer(id, ra).some,
              Seq.empty,
              remoteNetworkLevel = false,
              initialBlockReputation,
              initialPerfReputation,
              0
            )
          case Some(peer) => id -> peer.copy(asServer = RemotePeer(id, ra).some)
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

  def copyWithNetworkLevel(
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

  // Some cold peers can't be removed, for example if there is actor present OR remote peer is active
  def removeColdPeers(toRemove: Set[HostId]): PeersHandler[F] = {
    val filteredToRemove =
      getColdPeers.view
        .filterKeys(toRemove)
        .filterNot(_._2.isActive)
        .keySet
    val newPeers = peers -- filteredToRemove
    this.copy(peers = newPeers)
  }

  def copyWithClearedTimestamps: PeersHandler[F] = {
    val timeStampWindow = networkConfig.networkProperties.closeTimeoutWindowInMs
    val timeNow = System.currentTimeMillis()

    val newPeers = peers.map { case (id, peer) =>
      val eligibleTimestamps = peer.closedTimestamps.filter(_ >= (timeNow - timeStampWindow))
      id -> peer.copy(closedTimestamps = eligibleTimestamps)
    }
    this.copy(peers = newPeers)
  }
}

case class Peer[F[_]: Logger](
  state:              PeerState,
  actorOpt:           Option[PeerActor[F]],
  connectedAddress:   Option[RemoteAddress],
  asServer:           Option[RemotePeer],
  closedTimestamps:   Seq[Long],
  remoteNetworkLevel: Boolean,
  blockRep:           HostReputationValue,
  perfRep:            HostReputationValue,
  newRep:             Long
) {
  def couldOpenConnection: Boolean = asServer.isDefined || actorOpt.isDefined

  def isActive: Boolean = actorOpt.isDefined || remoteNetworkLevel

  def isUseful: Boolean = isActive || couldOpenConnection

  def haveNoConnection: Boolean = actorOpt.isEmpty

  def sendNoWait(message: PeerActor.Message): F[Unit] =
    actorOpt match {
      case Some(actor) => actor.sendNoWait(message)
      case None        => Logger[F].trace(show"Send message to peer with no running actor")
    }

  val reputation: Double = (blockRep + perfRep) / 2
}
