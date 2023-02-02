package co.topl.networking.fsnetwork

import cats.data.NonEmptyChain
import cats.effect.{Async, Concurrent, Resource}
import cats.implicits._
import co.topl.actor.{Actor, Fsm}
import co.topl.algebras.Store
import co.topl.consensus.algebras.LocalChainAlgebra
import co.topl.eventtree.ParentChildTree
import co.topl.models.{SlotData, Transaction, TypedIdentifier}
import co.topl.networking.blockchain.BlockchainPeerClient
import co.topl.networking.fsnetwork.BlockChecker.BlockCheckerActor
import co.topl.networking.fsnetwork.PeerActor.Message.{DownloadBlockBodies, DownloadBlockHeaders, UpdateState}
import co.topl.networking.fsnetwork.PeerBlockBodyFetcher.PeerBlockBodyFetcherActor
import co.topl.networking.fsnetwork.PeerBlockHeaderFetcher.PeerBlockHeaderFetcherActor
import co.topl.networking.fsnetwork.ReputationAggregator.ReputationAggregatorActor
import org.typelevel.log4cats.Logger

object PeerActor {
  sealed trait Message

  object Message {
    case class UpdateState(newState: PeerState) extends Message
    case class DownloadBlockHeaders(blockIds: NonEmptyChain[TypedIdentifier]) extends Message
    case class DownloadBlockBodies(blockIds: NonEmptyChain[TypedIdentifier]) extends Message
  }

  case class State[F[_]](
    hostId:               HostId,
    client:               BlockchainPeerClient[F],
    reputationAggregator: ReputationAggregatorActor[F],
    blockHeaderActor:     PeerBlockHeaderFetcherActor[F],
    blockBodyActor:       PeerBlockBodyFetcherActor[F]
  )

  type Response[F[_]] = State[F]
  type PeerActor[F[_]] = Actor[F, Message, Response[F]]

  def getFsm[F[_]: Concurrent]: Fsm[F, State[F], Message, Response[F]] = Fsm {
    case (state, UpdateState(newState))          => updateState(state, newState)
    case (state, DownloadBlockHeaders(blockIds)) => downloadHeaders(state, blockIds)
    case (state, DownloadBlockBodies(blockIds))  => downloadBodies(state, blockIds)
  }

  def makeActor[F[_]: Async: Logger](
    hostId:               HostId,
    client:               BlockchainPeerClient[F],
    reputationAggregator: ReputationAggregatorActor[F],
    blockChecker:         BlockCheckerActor[F],
    localChain:           LocalChainAlgebra[F],
    slotDataStore:        Store[F, TypedIdentifier, SlotData],
    transactionStore:     Store[F, TypedIdentifier, Transaction],
    blockIdTree:          ParentChildTree[F, TypedIdentifier]
  ): Resource[F, PeerActor[F]] =
    for {
      headerFetcher <-
        PeerBlockHeaderFetcher.makeActor(hostId, client, blockChecker, localChain, slotDataStore, blockIdTree)
      bodyFetcher <- PeerBlockBodyFetcher.makeActor(hostId, client, blockChecker, transactionStore)
      initialState = State(hostId, client, reputationAggregator, headerFetcher, bodyFetcher)
      thisActor <- Actor.make(initialState, getFsm[F])
    } yield thisActor

  // @TODO process state updating
  private def updateState[F[_]: Concurrent](state: State[F], newState: PeerState): F[(State[F], Response[F])] = {
    val applicationLevel: F[Unit] =
      if (newState.applicationLevel) {
        state.blockHeaderActor.sendNoWait(PeerBlockHeaderFetcher.Message.StartActor) *>
        state.blockBodyActor.sendNoWait(PeerBlockBodyFetcher.Message.StartActor)
      } else {
        state.blockHeaderActor.sendNoWait(PeerBlockHeaderFetcher.Message.StopActor) *>
        state.blockBodyActor.sendNoWait(PeerBlockBodyFetcher.Message.StopActor)
      }

    applicationLevel *> (state, state).pure[F]
  }

  private def downloadHeaders[F[_]: Concurrent](
    state:    State[F],
    blockIds: NonEmptyChain[TypedIdentifier]
  ): F[(State[F], Response[F])] =
    state.blockHeaderActor.sendNoWait(PeerBlockHeaderFetcher.Message.DownloadBlockHeaders(blockIds)) as (state, state)

  private def downloadBodies[F[_]: Concurrent](
    state:    State[F],
    blockIds: NonEmptyChain[TypedIdentifier]
  ): F[(State[F], Response[F])] =
    state.blockBodyActor.sendNoWait(PeerBlockBodyFetcher.Message.DownloadBlocks(blockIds)) as (state, state)
}
