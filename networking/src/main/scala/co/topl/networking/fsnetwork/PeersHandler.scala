package co.topl.networking.fsnetwork

import cats.Applicative
import cats.effect.Async
import cats.implicits._
import co.topl.networking.fsnetwork.PeerActor.PeerActor
import co.topl.networking.fsnetwork.PeersHandler.allowedTransition
import co.topl.networking.fsnetwork.ReputationAggregator.ReputationAggregatorActor
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
  peers:                Map[HostId, Peer[F]],
  timeStampWindow:      Long,
  reputationAggregator: Option[ReputationAggregatorActor[F]]
) {

  private def sendReputationMessage(message: ReputationAggregator.Message): F[Unit] =
    reputationAggregator.map(_.sendNoWait(message)).getOrElse(Applicative[F].unit)

  private def couldTransit(hostId: HostId, newState: PeerState): Boolean = {
    val currentState: PeerState = peers.get(hostId).map(_.state).getOrElse(PeerState.Unknown)
    allowedTransition(currentState).contains(newState)
  }

  def get(hostId: HostId): Option[Peer[F]] = peers.get(hostId)

  def apply(hostId: HostId): Peer[F] = peers(hostId)

  def haveNoActorForHost(hostId: HostId): Boolean = peers.get(hostId).flatMap(_.actorOpt).isEmpty

  private def hostIsBanned(hostId: HostId): Boolean = peers.get(hostId).exists(_.state == PeerState.Banned)

  def hostIsNotBanned(hostId: HostId): Boolean = !hostIsBanned(hostId)

  private def getPeers(peerState: PeerState): Map[HostId, Peer[F]] =
    peers.filter { case (_: String, peer) => peer.state == peerState }

  def getHotPeers: Map[HostId, Peer[F]] = getPeers(PeerState.Hot)

  def getWarmPeers: Map[HostId, Peer[F]] = getPeers(PeerState.Warm)

  def getAvailableToConnectAddresses: Set[RemoteAddress] =
    peers
      .filterNot(_._2.state == PeerState.Banned)
      .collect { case (hostId, Peer(_, _, Some(serverPort), _, _)) =>
        RemoteAddress(hostId, serverPort)
      }
      .toSet

  def getPeersWithPort(state: PeerState): Set[PeerWithHostAndPort[F]] =
    peers.collect { case (host, Peer(`state`, actor, Some(port), timestamps, _)) =>
      PeerWithHostAndPort(host, state, actor, port, timestamps)
    }.toSet

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
            _                       <- updateReputation(host, oldPeer, peerWithClosedTimestamp)
            actorOpt                <- closePeerIfNecessary(peerWithClosedTimestamp, peerActorCloseAndRelease)
            newPeer                 <- peerWithClosedTimestamp.copy(actorOpt = actorOpt).pure[F]
            _                       <- Logger[F].info(s"Move host $host with peer $oldPeer to new state $newPeer")
          } yield host -> newPeer
        }

    updateF.map(update => this.copy(peers = peers ++ update))
  }

  private def updateCloseTimestamps(peerToUpdate: Peer[F], newState: PeerState): Peer[F] =
    if (peerToUpdate.state.networkLevel && !newState.networkLevel) {
      val closeTimestamp = System.currentTimeMillis()
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

  private def updateReputation(hostId: HostId, oldPeer: Peer[F], newPeer: Peer[F]): F[Unit] = {
    val stopReputationTracking =
      if (oldPeer.state.isActive && !newPeer.state.isActive) {
        sendReputationMessage(ReputationAggregator.Message.StopReputationTracking(hostId))
      } else { Applicative[F].unit }

    val newHotPeer =
      if (!oldPeer.state.applicationLevel && newPeer.state.applicationLevel) {
        sendReputationMessage(ReputationAggregator.Message.NewHotPeer(hostId))
      } else { Applicative[F].unit }

    stopReputationTracking >> newHotPeer
  }

  private def closePeerIfNecessary(peer: Peer[F], peerActorRelease: Peer[F] => F[Unit]): F[Option[PeerActor[F]]] =
    if ((!peer.remoteNetworkLevel && !peer.state.networkLevel) || peer.state == PeerState.Banned) {
      peerActorRelease(peer) >>
      Option.empty[PeerActor[F]].pure[F]
    } else {
      peer.actorOpt.pure[F]
    }

  def copyWithAddedRemoteAddresses(newPeers: Set[RemoteAddress]): PeersHandler[F] =
    copyWithAddedHostAndPort(newPeers.map { case RemoteAddress(host, port) => (host, Option(port)) })

  def copyWithNewActor(host: HostId): PeersHandler[F] = {
    val peerToAdd =
      peers.get(host) match {
        case None              => host -> Peer(PeerState.Cold, None, None, Seq.empty, remoteNetworkLevel = false)
        case Some(currentPeer) => host -> currentPeer
      }
    this.copy(peers = peers + peerToAdd)
  }

  private def copyWithAddedHostAndPort(newPeers: Set[(HostId, Option[Int])]): PeersHandler[F] = {
    val peersToAdd = newPeers.map { case (host, port) =>
      peers.get(host) match {
        case None       => host -> Peer(PeerState.Cold, None, port, Seq.empty, remoteNetworkLevel = false)
        case Some(peer) => host -> peer.copy(remoteServerPort = port)
      }
    }.toMap

    this.copy(peers = peers ++ peersToAdd)
  }

  def copyWithUpdatedServerPort(hostId: HostId, serverPort: Int): PeersHandler[F] =
    peers
      .get(hostId)
      .map(peer => this.copy(peers = peers + (hostId -> peer.copy(remoteServerPort = Option(serverPort)))))
      .getOrElse(this)

  def copyWithUpdatedNetworkLevel(
    hostIds:          Set[HostId],
    netLevel:         Boolean,
    peerActorRelease: Peer[F] => F[Unit]
  ): F[PeersHandler[F]] = {
    val updatedPeersF =
      hostIds.flatMap(hostId => peers.get(hostId).map(peer => (hostId, peer))).toSeq.traverse { case (hostId, peer) =>
        val peerWithNetworkLevel = peer.copy(remoteNetworkLevel = netLevel)

        closePeerIfNecessary(peerWithNetworkLevel, peerActorRelease).map { actorOpt =>
          (hostId, peerWithNetworkLevel.copy(actorOpt = actorOpt))
        }
      }

    updatedPeersF.map(peers => this.copy(peers = this.peers ++ peers.toMap))
  }

  def copyWithNewPeerActor(hostId: HostId, peerActor: PeerActor[F]): F[PeersHandler[F]] =
    peers
      .get(hostId)
      .map { peer =>
        val hostAndPeer = hostId -> peer.copy(actorOpt = Option(peerActor))
        val state = hostAndPeer._2.state

        hostAndPeer._2.sendNoWait(PeerActor.Message.GetPeerServerAddress) >>
        hostAndPeer._2.sendNoWait(PeerActor.Message.UpdateState(state.networkLevel, state.applicationLevel)) >>
        this.copy(peers = peers + hostAndPeer).pure[F]
      }
      .getOrElse(Applicative[F].pure(this))
}

case class Peer[F[_]: Logger](
  state:              PeerState,
  actorOpt:           Option[PeerActor[F]],
  remoteServerPort:   Option[Int],
  closedTimestamps:   Seq[Long],
  remoteNetworkLevel: Boolean
) {
  def haveNoConnection: Boolean = actorOpt.isEmpty

  def sendNoWait(message: PeerActor.Message): F[Unit] =
    actorOpt match {
      case Some(actor) => actor.sendNoWait(message)
      case None        => Logger[F].debug(show"Send message to peer with no running client")
    }
}

case class PeerWithHostAndPort[F[_]](
  host:            HostId,
  state:           PeerState,
  actorOpt:        Option[PeerActor[F]],
  serverPort:      Int,
  closeTimestamps: Seq[Long]
) {
  val asRemoteAddress: RemoteAddress = RemoteAddress(host, serverPort)
}
