package co.topl.networking.fsnetwork

import cats.Applicative
import cats.effect.Async
import cats.implicits.showInterpolator
import co.topl.networking.fsnetwork.PeerActor.PeerActor
import co.topl.networking.fsnetwork.PeersHandler.allowedTransition
import co.topl.networking.p2p.RemoteAddress
import org.typelevel.log4cats.Logger
import cats.implicits._

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

case class PeersHandler[F[_]: Async: Logger](peers: Map[HostId, Peer[F]], timeStampWindow: Long) {

  private def couldTransit(hostId: HostId, newState: PeerState): Boolean = {
    val currentState: PeerState = peers.get(hostId).map(_.state).getOrElse(PeerState.Unknown)
    allowedTransition(currentState).contains(newState)
  }

  def get(hostId: HostId): Option[Peer[F]] = peers.get(hostId)

  def apply(hostId: HostId): Peer[F] = peers(hostId)

  def haveNoActorForHost(hostId: HostId): Boolean = peers.get(hostId).flatMap(_.actorOpt).isEmpty

  def hostIsBanned(hostId: HostId): Boolean = peers.get(hostId).exists(_.state == PeerState.Banned)

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

  def moveToState(forUpdate: HostId, newState: PeerState): F[(Map[HostId, Peer[F]], PeersHandler[F])] =
    moveToState(Set(forUpdate), newState)

  def moveToState(forUpdate: Set[HostId], newState: PeerState): F[(Map[HostId, Peer[F]], PeersHandler[F])] = {
    val updateF =
      forUpdate
        .filter(host => peers.contains(host) && couldTransit(host, newState))
        .map(host => (host, peers(host)))
        .toSeq
        .traverse { case (host, oldPeer) =>
          val newPeer = updateCloseTimestamps(oldPeer, newState).copy(state = newState)

          updateStateIfChanged(oldPeer, newPeer) >>
          closePeerIfNecessary(newPeer) >>
          Logger[F].info(s"Move host $host with peer $oldPeer to new state $newPeer") >>
          (host -> newPeer).pure[F]
        }

    updateF.map(update => (update.toMap, this.copy(peers = peers ++ update)))
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

  private def closePeerIfNecessary(peer: Peer[F]): F[Unit] =
    if (!peer.remoteNetworkLevel && !peer.state.networkLevel) {
      peer.sendNoWait(PeerActor.Message.CloseConnection)
    } else {
      Applicative[F].unit
    }

  def copyWithAddedRemoteAddresses(newPeers: Set[RemoteAddress]): PeersHandler[F] =
    copyWithAddedHostAndPort(newPeers.map { case RemoteAddress(host, port) => (host, Option(port)) })

  def copyWithAddedHost(host: HostId): PeersHandler[F] = {
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

  def copyWithUpdatedRemoteNetworkLevel(hostId: HostId, netLevel: Boolean): F[PeersHandler[F]] =
    peers
      .get(hostId)
      .traverse { peer =>
        val newPeer = peer.copy(remoteNetworkLevel = netLevel)

        closePeerIfNecessary(newPeer) >>
        this.copy(peers = peers + (hostId -> newPeer)).pure[F]
      }
      .flatMap {
        case Some(updatedPeer) =>
          updatedPeer.pure[F]
        case None =>
          Logger[F].error(s"Try to update remote app level for non exist peer $hostId") >> Applicative[F].pure(this)
      }

  def copyWithNewPeerActor(hostId: HostId, peerActor: PeerActor[F]): F[PeersHandler[F]] =
    peers
      .get(hostId)
      .map { peer =>
        val newPeer = hostId -> peer.copy(actorOpt = Option(peerActor))
        val state = newPeer._2.state

        newPeer._2.sendNoWait(PeerActor.Message.UpdateState(state.networkLevel, state.applicationLevel)) >>
        this.copy(peers = peers + newPeer).pure[F]
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
