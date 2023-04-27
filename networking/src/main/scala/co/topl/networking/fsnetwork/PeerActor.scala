package co.topl.networking.fsnetwork

import cats.data.NonEmptyChain
import cats.effect.{Async, Concurrent, Resource}
import cats.implicits._
import co.topl.actor.{Actor, Fsm}
import co.topl.algebras.Store
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.consensus.algebras.{BlockHeaderToBodyValidationAlgebra, LocalChainAlgebra}
import co.topl.consensus.models.{BlockHeader, BlockId, SlotData}
import co.topl.eventtree.ParentChildTree
import co.topl.networking.blockchain.BlockchainPeerClient
import co.topl.networking.fsnetwork.BlockChecker.BlockCheckerActor
import co.topl.networking.fsnetwork.PeerActor.Message._
import co.topl.networking.fsnetwork.PeerBlockBodyFetcher.PeerBlockBodyFetcherActor
import co.topl.networking.fsnetwork.PeerBlockHeaderFetcher.PeerBlockHeaderFetcherActor
import co.topl.networking.fsnetwork.RequestsProxy.RequestsProxyActor
import org.typelevel.log4cats.Logger

object PeerActor {
  sealed trait Message

  object Message {

    /**
     * Update peer state
     * @param newState new state
     */
    case class UpdateState(newState: PeerState) extends Message

    /**
     * Request to download block headers from peer, downloaded headers will be sent to block checker directly
     * @param blockIds headers block id to download
     */
    case class DownloadBlockHeaders(blockIds: NonEmptyChain[BlockId]) extends Message

    /**
     * Request to download block bodies from peer, downloaded bodies will be sent to block checker directly
     * @param blockData bodies block id to download
     */
    case class DownloadBlockBodies(blockData: NonEmptyChain[(BlockId, BlockHeader)]) extends Message

    /**
     * Request current tip from remote peer
     */
    case object GetCurrentTip extends Message
  }

  case class State[F[_]](
    hostId:           HostId,
    client:           BlockchainPeerClient[F],
    blockHeaderActor: PeerBlockHeaderFetcherActor[F],
    blockBodyActor:   PeerBlockBodyFetcherActor[F]
  )

  type Response[F[_]] = State[F]
  type PeerActor[F[_]] = Actor[F, Message, Response[F]]

  def getFsm[F[_]: Concurrent]: Fsm[F, State[F], Message, Response[F]] = Fsm {
    case (state, UpdateState(newState))          => updateState(state, newState)
    case (state, DownloadBlockHeaders(blockIds)) => downloadHeaders(state, blockIds)
    case (state, DownloadBlockBodies(blockIds))  => downloadBodies(state, blockIds)
    case (state, GetCurrentTip)                  => getCurrentTip(state)
  }

  def makeActor[F[_]: Async: Logger](
    hostId:                 HostId,
    client:                 BlockchainPeerClient[F],
    blockChecker:           BlockCheckerActor[F],
    requestsProxy:          RequestsProxyActor[F],
    localChain:             LocalChainAlgebra[F],
    slotDataStore:          Store[F, BlockId, SlotData],
    transactionStore:       Store[F, TransactionId, IoTransaction],
    blockIdTree:            ParentChildTree[F, BlockId],
    headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F]
  ): Resource[F, PeerActor[F]] =
    for {
      header <- PeerBlockHeaderFetcher.makeActor(
        hostId,
        client,
        blockChecker,
        requestsProxy,
        localChain,
        slotDataStore,
        blockIdTree
      )
      body <- PeerBlockBodyFetcher.makeActor(hostId, client, requestsProxy, transactionStore, headerToBodyValidation)
      initialState = State(hostId, client, header, body)
      actor <- Actor.make(initialState, getFsm[F])
    } yield actor

  private def updateState[F[_]: Concurrent](state: State[F], newState: PeerState): F[(State[F], Response[F])] = {
    val applicationLevel: F[Unit] =
      if (newState.applicationLevel) {
        state.blockHeaderActor.sendNoWait(PeerBlockHeaderFetcher.Message.StartActor) >>
        state.blockBodyActor.sendNoWait(PeerBlockBodyFetcher.Message.StartActor)
      } else {
        state.blockHeaderActor.sendNoWait(PeerBlockHeaderFetcher.Message.StopActor) >>
        state.blockBodyActor.sendNoWait(PeerBlockBodyFetcher.Message.StopActor)
      }

    applicationLevel >> (state, state).pure[F]
  }

  private def downloadHeaders[F[_]: Concurrent](
    state:    State[F],
    blockIds: NonEmptyChain[BlockId]
  ): F[(State[F], Response[F])] =
    state.blockHeaderActor.sendNoWait(PeerBlockHeaderFetcher.Message.DownloadBlockHeaders(blockIds)) >>
    (state, state).pure[F]

  private def downloadBodies[F[_]: Concurrent](
    state:     State[F],
    blockData: NonEmptyChain[(BlockId, BlockHeader)]
  ): F[(State[F], Response[F])] =
    state.blockBodyActor.sendNoWait(PeerBlockBodyFetcher.Message.DownloadBlocks(blockData)) >>
    (state, state).pure[F]

  private def getCurrentTip[F[_]: Concurrent](state: State[F]): F[(State[F], Response[F])] =
    state.blockHeaderActor.sendNoWait(PeerBlockHeaderFetcher.Message.GetCurrentTip) >>
    (state, state).pure[F]
}
