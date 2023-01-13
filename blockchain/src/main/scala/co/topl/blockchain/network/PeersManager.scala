package co.topl.blockchain.network

import cats.Applicative
import cats.data.NonEmptyChain
import cats.effect.{Concurrent, Resource, Temporal}
import cats.implicits.{catsSyntaxApplicativeId, toFlatMapOps, toFunctorOps}
import co.topl.blockchain.network.BlockBodiesChecker.BlockBodiesCheckerActor
import co.topl.blockchain.network.BlockHeadersChecker.BlockHeadersCheckerActor
import co.topl.blockchain.network.PeerActor.Message.UpdateState
import co.topl.blockchain.network.PeerActor.PeerActor
import co.topl.blockchain.network.PeersManager.Message.{BlockDownloadRequest, UpdatePeerStatus}
import co.topl.blockchain.network.ReputationAggregator.ReputationAggregatorActor
import co.topl.models.TypedIdentifier

object PeersManager {
  sealed trait Message

  object Message {
    case class UpdatePeerStatus(hostId: HostId, newState: PeerState) extends Message
    // use hostId as hint for now, later we could have some kind of map of available peer -> blocks
    // (we have it because of headers) and send requests to many peers,
    // that functionality even could be extracted to other actor
    case class BlockDownloadRequest(hostId: HostId, blocks: NonEmptyChain[TypedIdentifier]) extends Message
  }

  case class Peer[F[_]](state: PeerState, actorOpt: Option[PeerActor[F]])

  case class State[F[_]](
    networkAlgebra:       NetworkAlgebra[F],
    reputationAggregator: ReputationAggregatorActor[F],
    peers:                Map[HostId, Peer[F]],
    blockHeadersChecker:  BlockHeadersCheckerActor[F],
    blockBodiesChecker:   BlockBodiesCheckerActor[F]
  )

  type Response[F[_]] = State[F]
  type PeersManagerActor[F[_]] = Actor[F, Message, Response[F]]

  def getFsm[F[_]: Concurrent: Temporal]: PeersManagerActor[F] => Fsm[F, State[F], Message, Response[F]] = thisActor =>
    Fsm {
      case (currentState, update: UpdatePeerStatus)             => updatePeerStatus(thisActor, currentState, update)
      case (currentState, BlockDownloadRequest(hostId, blocks)) => blockDownloadRequest(currentState, hostId, blocks)
    }

  def makeActor[F[_]: Concurrent: Temporal](
    networkAlgebra:            NetworkAlgebra[F],
    reputationAggregatorActor: ReputationAggregatorActor[F],
    blockHeadersChecker:       BlockHeadersCheckerActor[F],
    blockBodiesChecker:        BlockBodiesCheckerActor[F]
  ): Resource[F, PeersManagerActor[F]] = {
    val initialState =
      PeersManager.State[F](
        networkAlgebra,
        reputationAggregatorActor,
        Map.empty[HostId, Peer[F]],
        blockHeadersChecker,
        blockBodiesChecker
      )
    Actor.makeFull(initialState, getFsm[F], finalizeActor[F])
  }

  private def updatePeerStatus[F[_]: Concurrent: Temporal](
    thisActor: PeersManagerActor[F],
    state:     State[F],
    newStatus: UpdatePeerStatus
  ): F[(State[F], Response[F])] = {
    val hostId: HostId = newStatus.hostId
    val peer: Peer[F] = state.peers.getOrElse(hostId, Peer[F](PeerState.Cold, None))

    val peerActorF: F[Option[PeerActor[F]]] =
      (newStatus.newState.activeActor, peer.actorOpt) match {
        case (true, None) =>
          thisActor.acquireActor(() =>
            PeerActor.makeActor(hostId, state.reputationAggregator, state.blockHeadersChecker, state.blockBodiesChecker)
          ).map(Some(_))
        case (false, Some(actorToShutdown)) =>
          thisActor.removeActor(actorToShutdown).map(_ => None)
        case _ => peer.actorOpt.pure[F]
      }

    for {
      peerWithActor <- peerActorF.map(peerActor => peer.copy(state= newStatus.newState, actorOpt = peerActor))
      _ <- peerWithActor.actorOpt
        .map(_.sendNoWait(PeerActor.Message.UpdateState(newStatus.newState)))
        .getOrElse(().pure)
      newPeers = state.peers + (hostId -> peerWithActor)
      newState = state.copy(peers = newPeers)
    } yield (newState, newState)
  }

  private def blockDownloadRequest[F[_]: Applicative](
    state:           State[F],
    requestedHostId: HostId,
    blocks:          NonEmptyChain[TypedIdentifier]
  ): F[(State[F], Response[F])] =
    state.peers.get(requestedHostId) match {
      case Some(peer) =>
        //TODO add logic if no peer actor is present
        peer.actorOpt.map(_.sendNoWait(PeerActor.Message.DownloadBlocksRequest(blocks)))
        (state, state).pure[F]
      case None => ???
    }

  private def finalizeActor[F[_]: Applicative](currentState: State[F]): F[Unit] = ().pure[F]

}
