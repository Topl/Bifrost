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
import co.topl.networking.fsnetwork.PeerActor.Message._
import co.topl.networking.fsnetwork.PeerBlockBodyFetcher.PeerBlockBodyFetcherActor
import co.topl.networking.fsnetwork.PeerBlockHeaderFetcher.PeerBlockHeaderFetcherActor
import co.topl.networking.fsnetwork.PeerNetworkQuality.PeerNetworkQualityActor
import co.topl.networking.fsnetwork.ReputationAggregator.ReputationAggregatorActor
import co.topl.networking.fsnetwork.RequestsProxy.RequestsProxyActor
import org.typelevel.log4cats.Logger

object PeerActor {
  sealed trait Message

  object Message {

    /**
     * Update peer state
     * @param networkLevel do we run network level data exchange like measuring metwork quality
     * @param applicationLevel do we run application level data exchange
     */
    case class UpdateState(networkLevel: Boolean, applicationLevel: Boolean) extends Message

    /**
     * Request to download block headers from peer, downloaded headers will be sent to block checker directly
     * @param blockIds headers block id to download
     */
    case class DownloadBlockHeaders(blockIds: NonEmptyChain[BlockId]) extends Message

    /**
     * Request to download block bodies from peer, downloaded bodies will be sent to block checker directly
     * @param blockData bodies block id to download
     */
    case class DownloadBlockBodies(blockData: NonEmptyChain[BlockHeader]) extends Message

    /**
     * Request current tip from remote peer
     */
    case object GetCurrentTip extends Message

    /**
     * Request network quality measure
     */
    case object GetNetworkQuality extends Message

    /**
     * Request to get remote host's hot connections
     */
    case object GetHotPeersFromPeer extends Message
  }

  case class State[F[_]](
    hostId:              HostId,
    client:              BlockchainPeerClient[F],
    blockHeaderActor:    PeerBlockHeaderFetcherActor[F],
    blockBodyActor:      PeerBlockBodyFetcherActor[F],
    networkQualityActor: PeerNetworkQualityActor[F],
    networkLevel:        Boolean,
    applicationLevel:    Boolean
  )

  type Response[F[_]] = State[F]
  type PeerActor[F[_]] = Actor[F, Message, Response[F]]

  def getFsm[F[_]: Concurrent: Logger]: Fsm[F, State[F], Message, Response[F]] = Fsm {
    case (state, UpdateState(networkLevel, applicationLevel)) => updateState(state, networkLevel, applicationLevel)
    case (state, DownloadBlockHeaders(blockIds))              => downloadHeaders(state, blockIds)
    case (state, DownloadBlockBodies(blockHeaders))           => downloadBodies(state, blockHeaders)
    case (state, GetCurrentTip)                               => getCurrentTip(state)
    case (state, GetNetworkQuality)                           => getNetworkQuality(state)
    case (state, GetHotPeersFromPeer)                         => getHotPeers(state)
  }

  def makeActor[F[_]: Async: Logger](
    hostId:                 HostId,
    client:                 BlockchainPeerClient[F],
    requestsProxy:          RequestsProxyActor[F],
    reputationAggregator:   ReputationAggregatorActor[F],
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
        requestsProxy,
        localChain,
        slotDataStore,
        blockIdTree
      )
      body <- PeerBlockBodyFetcher.makeActor(hostId, client, requestsProxy, transactionStore, headerToBodyValidation)
      networkQuality <- PeerNetworkQuality.makeActor(hostId, client, reputationAggregator)
      initialState = State(hostId, client, header, body, networkQuality, networkLevel = false, applicationLevel = false)
      actor <- Actor.makeWithFinalize(initialState, getFsm[F], finalizer[F])
    } yield actor

  private def finalizer[F[_]: Concurrent: Logger](state: State[F]): F[Unit] =
    Logger[F].info(show"Finishing actor for peer ${state.hostId}") >>
    stopApplicationLevel(state) >>
    stopNetworkLevel(state)

  private def updateState[F[_]: Concurrent: Logger](
    state:               State[F],
    newNetworkLevel:     Boolean,
    newApplicationLevel: Boolean
  ): F[(State[F], Response[F])] = {
    val networkLevel: F[Unit] =
      (state.networkLevel != newNetworkLevel, newNetworkLevel) match {
        case (true, true)  => startNetworkLevel(state)
        case (true, false) => stopNetworkLevel(state)
        case (false, _)    => ().pure[F]
      }

    val applicationLevel: F[Unit] =
      (state.applicationLevel != newApplicationLevel, newApplicationLevel) match {
        case (true, true)  => startApplicationLevel(state)
        case (true, false) => stopApplicationLevel(state)
        case (false, _)    => ().pure[F]
      }

    val newState: State[F] = state.copy(applicationLevel = newApplicationLevel, networkLevel = newNetworkLevel)
    applicationLevel >> networkLevel >> (newState, newState).pure[F]
  }

  private def startApplicationLevel[F[_]: Concurrent: Logger](state: State[F]): F[Unit] =
    Logger[F].info(show"Application level is enabled for ${state.hostId}") >>
    state.blockHeaderActor.sendNoWait(PeerBlockHeaderFetcher.Message.StartActor) >>
    state.blockBodyActor.sendNoWait(PeerBlockBodyFetcher.Message.StartActor)

  private def stopApplicationLevel[F[_]: Concurrent: Logger](state: State[F]): F[Unit] =
    Logger[F].info(show"Application level is disabled for ${state.hostId}") >>
    state.blockHeaderActor.sendNoWait(PeerBlockHeaderFetcher.Message.StopActor) >>
    state.blockBodyActor.sendNoWait(PeerBlockBodyFetcher.Message.StopActor)

  private def startNetworkLevel[F[_]: Concurrent: Logger](state: State[F]): F[Unit] =
    Logger[F].info(show"Network level is started for ${state.hostId}") >>
    state.networkQualityActor.sendNoWait(PeerNetworkQuality.Message.StartActor)

  private def stopNetworkLevel[F[_]: Concurrent: Logger](state: State[F]): F[Unit] =
    Logger[F].info(show"Network level is stop for ${state.hostId}") >>
    state.networkQualityActor.sendNoWait(PeerNetworkQuality.Message.StopActor)

  private def downloadHeaders[F[_]: Concurrent](
    state:    State[F],
    blockIds: NonEmptyChain[BlockId]
  ): F[(State[F], Response[F])] =
    state.blockHeaderActor.sendNoWait(PeerBlockHeaderFetcher.Message.DownloadBlockHeaders(blockIds)) >>
    (state, state).pure[F]

  private def downloadBodies[F[_]: Concurrent](
    state:     State[F],
    blockData: NonEmptyChain[BlockHeader]
  ): F[(State[F], Response[F])] =
    state.blockBodyActor.sendNoWait(PeerBlockBodyFetcher.Message.DownloadBlocks(blockData)) >>
    (state, state).pure[F]

  private def getCurrentTip[F[_]: Concurrent](state: State[F]): F[(State[F], Response[F])] =
    state.blockHeaderActor.sendNoWait(PeerBlockHeaderFetcher.Message.GetCurrentTip) >>
    (state, state).pure[F]

  private def getNetworkQuality[F[_]: Concurrent](state: State[F]): F[(State[F], Response[F])] =
    state.networkQualityActor.sendNoWait(PeerNetworkQuality.Message.GetNetworkQuality) >>
    (state, state).pure[F]

  private def getHotPeers[F[_]: Concurrent](state: State[F]): F[(State[F], Response[F])] =
    (state, state).pure[F]
}
