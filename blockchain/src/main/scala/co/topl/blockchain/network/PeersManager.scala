package co.topl.blockchain.network

import cats.Applicative
import cats.data.NonEmptyChain
import cats.effect.{Concurrent, Resource}
import cats.implicits.{catsSyntaxApplicativeId, toFlatMapOps, toFunctorOps}
import co.topl.blockchain.network.BlockBodiesChecker.BlockBodiesCheckerActor
import co.topl.blockchain.network.BlockHeadersChecker.BlockHeadersCheckerActor
import co.topl.blockchain.network.Peer.Message.UpdateState
import co.topl.blockchain.network.Peer.PeerActor
import co.topl.blockchain.network.PeersManager.Message.{BlockDownloadRequest, UpdatePeerStatus}
import co.topl.blockchain.network.PeerState.PeerState
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

  case class State[F[_]](
    networkAlgebra:       NetworkAlgebra[F],
    reputationAggregator: ReputationAggregatorActor[F],
    peers:                Map[HostId, (PeerActor[F], F[Unit])],
    blockHeadersChecker:  BlockHeadersCheckerActor[F],
    blockBodiesChecker:   BlockBodiesCheckerActor[F]
  )

  type Response[F[_]] = State[F]
  type PeersManagerActor[F[_]] = Actor[F, Message, Response[F]]

  def getFsm[F[_]: Concurrent]: Fsm[F, State[F], Message, Response[F]] =
    Fsm {
      case (currentState, update: UpdatePeerStatus) => updatePeerStatus(currentState, update)
      case (currentState, BlockDownloadRequest(hostId, blocks)) => blockDownloadRequest(currentState, hostId, blocks)
    }

  def makeActor[F[_]: Concurrent](
    networkAlgebra:            NetworkAlgebra[F],
    reputationAggregatorActor: ReputationAggregatorActor[F],
    blockHeadersChecker:       BlockHeadersCheckerActor[F],
    blockBodiesChecker:        BlockBodiesCheckerActor[F]
  ): Resource[F, PeersManagerActor[F]] = {
    val initialState =
      PeersManager.State(
        networkAlgebra,
        reputationAggregatorActor,
        Map.empty[HostId, (PeerActor[F], F[Unit])],
        blockHeadersChecker,
        blockBodiesChecker
      )
    Actor.makeWithFinalize(initialState, getFsm[F], finalizeActor[F])
  }

  private def updatePeerStatus[F[_]: Concurrent](
    state:     State[F],
    newStatus: UpdatePeerStatus
  ): F[(State[F], Response[F])] = {
    def getOrBuildPeer: F[(PeerActor[F], F[Unit])] = state.peers.get(newStatus.hostId).pure.flatMap {
      case Some(peer) => peer.pure
      case None =>
        state.networkAlgebra
          .makePeer(newStatus.hostId, state.reputationAggregator, state.blockHeadersChecker, state.blockBodiesChecker)
          .allocated

    }

    for {
      (peerActor: PeerActor[F], finalizer) <- getOrBuildPeer
      _ = peerActor.sendNoWait(UpdateState(newStatus.newState))
      updatedPeers: Map[HostId, (PeerActor[F], F[Unit])] = state.peers + (newStatus.hostId -> (peerActor, finalizer))
      newState = state.copy(peers = updatedPeers)
    } yield (newState, newState)
  }

  private def blockDownloadRequest[F[_]: Applicative](
    state:           State[F],
    requestedHostId: HostId,
    blocks:          NonEmptyChain[TypedIdentifier]
  ): F[(State[F], Response[F])] =
    state.peers.get(requestedHostId) match {
      case Some(peer) =>
        peer._1.sendNoWait(Peer.Message.DownloadBlocksRequest(blocks))
        (state, state).pure[F]
      case None => ???
    }

  private def finalizeActor[F[_]: Applicative](currentState: State[F]): F[Unit] =
    currentState.peers.values.foreach(_._1.sendNoWait(UpdateState(PeerState.Cold))).pure

}
