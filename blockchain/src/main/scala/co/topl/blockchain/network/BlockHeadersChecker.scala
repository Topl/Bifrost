package co.topl.blockchain.network

import cats.Applicative
import cats.data.NonEmptyChain
import cats.effect.Resource
import cats.effect.kernel.Concurrent
import cats.implicits.catsSyntaxApplicativeId
import co.topl.blockchain.actor.{Actor, Fsm}
import co.topl.blockchain.network.BlockBodiesChecker.BlockBodiesCheckerActor
import co.topl.blockchain.network.BlockHeadersChecker.Message.{RemoteHeaderChain, RemoveHeadersFromHostId}
import co.topl.blockchain.network.ReputationAggregator.ReputationAggregatorActor
import co.topl.models.TypedIdentifier

/**
 * Got headers, from peers. Responsible for:
 * verifying header chain
 * selecting best candidate header
 */
object BlockHeadersChecker {
  sealed trait Message

  object Message {
    case class RemoteHeaderChain(proposal: HeadersCandidate) extends Message
    // need for not being stuck with chain with valid headers but invalid bodies
    case class RemoveHeadersFromHostId(hostId: HostId) extends Message
  }

  case class State[F[_]](
    reputationAggregator: ReputationAggregatorActor[F],
    blockBodyFetcher:     BlockBodiesCheckerActor[F],
    // hostId for feedback about host, shall we store possible candidates as well?
    bestHeadersChain: Option[(HostId, NonEmptyChain[TypedIdentifier])]
  )

  type Response[F[_]] = State[F]
  type BlockHeadersCheckerActor[F[_]] = Actor[F, Message, Response[F]]

  def getFsm[F[_]: Applicative]: Fsm[F, State[F], Message, Response[F]] =
    Fsm {
      case (currentState, remoteChain: RemoteHeaderChain) => addNewChain(currentState, remoteChain)
      case (state, RemoveHeadersFromHostId(hostId))       => removeHeadersFromHostId(state, hostId)
    }

  def makeActor[F[_]: Concurrent](
    reputationAggregator: ReputationAggregatorActor[F],
    blockBodyChecker:     BlockBodiesCheckerActor[F]
  ): Resource[F, BlockHeadersCheckerActor[F]] = {
    val initialState = State(reputationAggregator, blockBodyChecker, None)
    Actor.make(initialState, getFsm[F])
  }

  // TODO shall be done in background
  private def addNewChain[F[_]: Applicative](
    state:             State[F],
    remoteHeaderChain: RemoteHeaderChain
  ): F[(State[F], Response[F])] = {
    val candidateHostId = remoteHeaderChain.proposal.source
    val candidateChain = remoteHeaderChain.proposal.headers

    val newBestChain: Option[(HostId, NonEmptyChain[TypedIdentifier])] =
      if (state.bestHeadersChain.exists(c => leftBetterThanRight(c._2, candidateChain))) {
        state.bestHeadersChain
      } else {
        if (isValidChain(candidateChain)) {
          state.reputationAggregator.sendNoWait(
            ReputationAggregator.Message.UpdatePeerReputation(candidateHostId, 1)
          ) // 1 as example here
          state.blockBodyFetcher.sendNoWait(
            BlockBodiesChecker.Message.HeadersProposal(HeadersCandidate(candidateHostId, candidateChain))
          )
          Some((candidateHostId, candidateChain))
        } else {
          state.reputationAggregator.sendNoWait(
            ReputationAggregator.Message.UpdatePeerReputation(candidateHostId, -1)
          ) // 1 as example here
          state.bestHeadersChain
        }
      }
    val newState = state.copy(bestHeadersChain = newBestChain)
    (newState, newState).pure[F]
  }

  private def removeHeadersFromHostId[F[_]: Applicative](state: State[F], hostId: HostId) = {
    val bestChain =
      if (state.bestHeadersChain.exists(_._1 == hostId)) {
        None
      } else {
        state.bestHeadersChain
      }

    val newState = state.copy(bestHeadersChain = bestChain)
    (newState, newState).pure[F]
  }

  private def leftBetterThanRight(left: NonEmptyChain[TypedIdentifier], right: NonEmptyChain[TypedIdentifier]) = true

  private def isValidChain(chain: NonEmptyChain[TypedIdentifier]) = true

}
