package co.topl.blockchain.network

import cats.Applicative
import cats.data.NonEmptyChain
import cats.effect.{Concurrent, Resource}
import cats.implicits.catsSyntaxApplicativeId
import co.topl.blockchain.network
import co.topl.blockchain.network.BlockBodiesChecker.BlockBodiesCheckerActor
import co.topl.models.TypedIdentifier

object PeerBlockBodyFetcher {
  sealed trait Message

  object Message {
    case class DownloadBlocks(blocks: NonEmptyChain[TypedIdentifier]) extends Message
    case object StopActor extends Message
    case object StartActor extends Message
  }

  case class State[F[_]](hostId: HostId, blockBodiesChecker: BlockBodiesCheckerActor[F])

  type Response[F[_]] = State[F]
  type PeerBlockBodyFetcherActor[F[_]] = Actor[F, Message, Response[F]]

  def getFsm[F[_]: Concurrent]: Fsm[F, State[F], Message, Response[F]] = Fsm {
    case (state, Message.DownloadBlocks(blocksToDownload)) => downloadBlocks(state, blocksToDownload)
    case (state, Message.StopActor)                        => stopActor(state)
  }

  def makeActor[F[_]: Concurrent](
    hostId:             HostId,
    blockBodiesChecker: BlockBodiesCheckerActor[F]
  ): Resource[F, PeerBlockBodyFetcherActor[F]] = {
    val initialState = State(hostId, blockBodiesChecker)
    Actor.make(initialState, getFsm[F])
  }

  private def downloadBlocks[F[_]: Applicative](state: State[F], blocksToDownload: NonEmptyChain[TypedIdentifier]): F[(State[F], Response[F])] = {
    // download blocks in background mode
    val downloadedBlocks: NonEmptyChain[ErrorOrBlock] = ???
    state.blockBodiesChecker.sendNoWait(BlockBodiesChecker.Message.DownloadedBlocks(downloadedBlocks))
    (state, state).pure[F]
  }

  private def stopActor[F[_]: Applicative](state: State[F]): F[(State[F], Response[F])] =
    (state, state).pure[F]
}
