package co.topl.blockchain.network

import cats.Applicative
import cats.data.NonEmptyChain
import cats.effect.{Concurrent, Resource}
import cats.implicits.{catsSyntaxApplicativeId, catsSyntaxApply}
import co.topl.blockchain.network.BlockBodiesChecker.BlockBodiesCheckerActor
import co.topl.blockchain.network.BlockHeadersChecker.BlockHeadersCheckerActor
import co.topl.blockchain.network.PeerActor.Message.UpdateState
import co.topl.blockchain.network.PeerBlockBodyFetcher.PeerBlockBodyFetcherActor
import co.topl.blockchain.network.PeerBlockHeaderFetcher.PeerBlockHeaderFetcherActor
import co.topl.blockchain.network.ReputationAggregator.ReputationAggregatorActor
import co.topl.models.TypedIdentifier

object PeerActor {
  sealed trait Message

  object Message {
    case class UpdateState(newState: PeerState) extends Message
    case class DownloadBlocksRequest(headers: NonEmptyChain[TypedIdentifier]) extends Message
  }

  case class State[F[_]](
    hostId:               HostId,
    reputationAggregator: ReputationAggregatorActor[F],
    blockHeaderActor:     PeerBlockHeaderFetcherActor[F],
    blockBodyActor:       PeerBlockBodyFetcherActor[F]
  )

  type Response[F[_]] = State[F]
  type PeerActor[F[_]] = Actor[F, Message, Response[F]]

  def getFsm[F[_]: Concurrent]: Fsm[F, State[F], Message, Response[F]] = Fsm {
    case (state, UpdateState(newState)) => updateState(state, newState)
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
      initialState = State(hostId, reputationAggregator, headerFetcher, bodyFetcher)
      thisActor <- Actor.make(initialState, getFsm[F])
    } yield thisActor

  private def finalizeActor[F[_]: Applicative](currentState: State[F]): F[Unit] =
    ().pure[F]

  private def updateState[F[_]: Concurrent](state: State[F], newState: PeerState): F[(State[F], Response[F])] = {
    val applicationLevel: F[Unit] =
      if (newState.applicationLevel) {
        state.blockHeaderActor.sendNoWait(PeerBlockHeaderFetcher.Message.StartActor) *>
        state.blockBodyActor.sendNoWait(PeerBlockBodyFetcher.Message.StartActor)
      } else {
        state.blockHeaderActor.sendNoWait(PeerBlockHeaderFetcher.Message.StartActor) *>
        state.blockBodyActor.sendNoWait(PeerBlockBodyFetcher.Message.StartActor)
      }

    applicationLevel *> (state, state).pure[F]
  }
}
