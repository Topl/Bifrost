package co.topl.blockchain.network

import cats.Applicative
import cats.data.NonEmptyChain
import cats.effect.{Concurrent, Resource}
import cats.implicits.catsSyntaxApplicativeId
import co.topl.blockchain.network.BlockBodiesChecker.BlockBodiesCheckerActor
import co.topl.blockchain.network.BlockHeadersChecker.BlockHeadersCheckerActor
import co.topl.blockchain.network.Peer.Message.{DownloadBlocksRequest, UpdateState}
import co.topl.blockchain.network.PeerBlockBodyFetcher.PeerBlockBodyFetcherActor
import co.topl.blockchain.network.PeerBlockHeaderFetcher.PeerBlockHeaderFetcherActor
import co.topl.blockchain.network.PeerState.PeerState
import co.topl.blockchain.network.ReputationAggregator.ReputationAggregatorActor
import co.topl.models.TypedIdentifier

object Peer {
  sealed trait Message

  object Message {
    case class UpdateState(newState: PeerState) extends Message
    case class DownloadBlocksRequest(headers: NonEmptyChain[TypedIdentifier]) extends Message
  }

  case class State[F[_]](
    connectionState:      ConnectionState,
    hostId:               HostId,
    reputationAggregator: ReputationAggregatorActor[F],
    blockHeaderActor:     PeerBlockHeaderFetcherActor[F],
    blockBodyActor:       PeerBlockBodyFetcherActor[F]
  )

  type Response[F[_]] = State[F]
  type PeerActor[F[_]] = Actor[F, Message, Response[F]]

  sealed trait ConnectionState

  //thinking about creating different Peer, f.e PeerCold, PeerHot, etc.
  object ConnectionState {
    case object Banned extends ConnectionState
    case object Cold extends ConnectionState
    case object Warm extends ConnectionState
    case object Hot extends ConnectionState
  }

  def getFsm[F[_]: Concurrent]: Fsm[F, State[F], Message, Response[F]] = Fsm {
    case (state @ State(_: ConnectionState.Banned.type, _, _, _, _), message) =>
      processMessageForBannedState(state, message)
    case (state @ State(_: ConnectionState.Cold.type, _, _, _, _), message) =>
      processMessageForBannedState(state, message)
    case (state @ State(_: ConnectionState.Warm.type, _, _, _, _), message) =>
      processMessageForBannedState(state, message)
    case (state @ State(_: ConnectionState.Hot.type, _, _, _, _), message) =>
      processMessageForBannedState(state, message)
  }

  def makeActor[F[_]: Concurrent](
    hostId:               HostId,
    reputationAggregator: ReputationAggregatorActor[F],
    blockHeaderChecker:   BlockHeadersCheckerActor[F],
    blockBodiesChecker:   BlockBodiesCheckerActor[F]
  ): Resource[F, PeerActor[F]] =
    for {
      headerFetcher <- PeerBlockHeaderFetcher.makeActor(hostId, blockHeaderChecker)
      bodyFetcher   <- PeerBlockBodyFetcher.makeActor(hostId, blockBodiesChecker)
      initialState = State(ConnectionState.Cold, hostId, reputationAggregator, headerFetcher, bodyFetcher)
      thisActor <- Actor.make(initialState, getFsm[F])
    } yield thisActor

  private def finalizeActor[F[_]: Applicative](currentState: State[F]) = {
    currentState match {
      case state @ State(_: ConnectionState.Hot.type, _, _, _, _) => stopHotPeer(state)
      case _                                                      =>
    }

    ().pure[F]
  }

  private def processMessageForBannedState[F[_]: Applicative](state: State[F], message: Message) =
    (state, state).pure[F]

  private def processMessageForColdState[F[_]: Applicative](state: State[F], message: Message) =
    (state, state).pure[F]

  private def processMessageForWarmState[F[_]: Applicative](state: State[F], message: Message) =
    (state, state).pure[F]

  private def processMessageForWarmState[F[_]: Applicative](state: State[F], message: Message) =
    message match {
      case UpdateState(PeerState.Banned) => stopHotPeer(state)
      case UpdateState(PeerState.Cold)   => stopHotPeer(state)
      case UpdateState(PeerState.Warm)   => ???
      case UpdateState(PeerState.Hot)    => ???

      case DownloadBlocksRequest(headers) =>
        state.blockBodyActor.sendNoWait(PeerBlockBodyFetcher.Message.DownloadBlocks(headers))
        (state, state).pure[F]
    }

  private def stopHotPeer[F[_]: Applicative](state: State[F]) = {
    state.blockHeaderActor.sendNoWait(PeerBlockHeaderFetcher.Message.StopActor)
    state.blockBodyActor.sendNoWait(PeerBlockBodyFetcher.Message.StopActor)
    (state, state).pure[F]
  }
}
