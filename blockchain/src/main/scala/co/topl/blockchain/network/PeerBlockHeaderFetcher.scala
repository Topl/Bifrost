package co.topl.blockchain.network

import cats.Applicative
import cats.data.NonEmptyChain
import cats.effect.{Concurrent, Resource}
import cats.implicits.catsSyntaxApplicativeId
import co.topl.blockchain.network.BlockHeadersChecker.BlockHeadersCheckerActor
import co.topl.models.TypedIdentifier

object PeerBlockHeaderFetcher {
  sealed trait Message

  object Message {
    case object StartActor extends Message
    case object StopActor extends Message
  }

  case class State[F[_]](hostId: HostId, blockHeadersChecker: BlockHeadersCheckerActor[F])

  type Response[F[_]] = State[F]
  type PeerBlockHeaderFetcherActor[F[_]] = Actor[F, Message, Response[F]]

  def getFsm[F[_]: Concurrent]: Fsm[F, State[F], Message, Response[F]] = Fsm {
    case (state, Message.StartActor) => startActor(state)
    case (state, Message.StopActor)  => stopActor(state)
  }

  def makeActor[F[_]: Concurrent](
    hostId:              HostId,
    blockHeadersChecker: BlockHeadersCheckerActor[F]
  ): Resource[F, Actor[F, Message, Response[F]]] = {
    val initialState = State(hostId, blockHeadersChecker)
    Actor.make(initialState, getFsm[F])
  }

  private def startActor[F[_]: Applicative](state: State[F]): F[(State[F], Response[F])] = {
    // do it in background
    val downloadedHeaders: NonEmptyChain[TypedIdentifier] = ???
    val headersCandidate: HeadersCandidate = HeadersCandidate(state.hostId, downloadedHeaders)
    state.blockHeadersChecker.sendNoWait(BlockHeadersChecker.Message.RemoteHeaderChain(headersCandidate))
    // end background task
    (state, state).pure[F]
  }
  private def stopActor[F[_]: Applicative](state: State[F]): F[(State[F], Response[F])] = ???

}
