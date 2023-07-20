package co.topl.networking.fsnetwork

import cats.Applicative
import cats.data.NonEmptyChain
import cats.effect.Async
import cats.effect.Resource
import cats.implicits._
import co.topl.actor.Actor
import co.topl.actor.Fsm
import co.topl.algebras.Store
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import co.topl.consensus.algebras.{BlockHeaderToBodyValidationAlgebra, LocalChainAlgebra}
import co.topl.consensus.models.{BlockHeader, BlockId, SlotData}
import co.topl.eventtree.ParentChildTree
import co.topl.networking.blockchain.BlockchainPeerClient
import co.topl.networking.fsnetwork.BlockChecker.BlockCheckerActor
import co.topl.networking.fsnetwork.PeerActor.PeerActor
import co.topl.networking.fsnetwork.PeersManager.Message._
import co.topl.networking.fsnetwork.ReputationAggregator.ReputationAggregatorActor
import co.topl.networking.fsnetwork.RequestsProxy.RequestsProxyActor
import org.typelevel.log4cats.Logger

/**
 * Actor for managing peers
 */
object PeersManager {
  sealed trait Message

  object Message {

    /**
     * Setup appropriate actor for connection to peer specified by hostId, client shall be created outside
     * @param hostId host to connect
     * @param client client with already opened connection to host
     *               TODO client shall be created inside PeersManager
     * @tparam F effect
     */
    case class SetupPeer[F[_]](hostId: HostId, client: BlockchainPeerClient[F]) extends Message

    /**
     * Set block checker actor, can't be done in constructor due cyclic references
     * @param blockChecker block checker
     * @tparam F effect
     */
    case class SetupBlockChecker[F[_]](blockChecker: BlockCheckerActor[F]) extends Message

    /**
     * Set requests proxy actor, can't be done in constructor due cyclic references
     *
     * @param requestsProxy block checker
     * @tparam F effect
     */
    case class SetupRequestsProxy[F[_]](requestsProxy: RequestsProxyActor[F]) extends Message

    /**
     * Set reputation aggregator actor, can't be done in constructor due cyclic references
     * @param aggregator reputation aggregator
     * @tparam F effect
     */
    case class SetupReputationAggregator[F[_]](aggregator: ReputationAggregatorActor[F]) extends Message

    /**
     * Update peer status due to some external event like changed reputation status
     * @param hostId peer to be affected
     * @param newState new state
     */
    case class UpdatePeerStatus(hostId: HostId, newState: PeerState) extends Message

    /**
     * @param hostId use hostId as hint for now, later we could have some kind of map of available peer -> headers
     * @param blockIds list of block's id of headers to be requested from peer
     */
    case class BlockHeadersRequest(hostId: HostId, blockIds: NonEmptyChain[BlockId]) extends Message

    /**
     * @param hostId use hostId as hint for now, later we could have some kind of map of available peer -> blocks
     *               and send requests to many peers
     * @param blockHeaders requested bodies
     */
    case class BlockBodyRequest(hostId: HostId, blockHeaders: NonEmptyChain[BlockHeader]) extends Message

    /**
     * Request current tips from all connected hot peers
     */
    case object GetCurrentTips extends Message

    /**
     * Request current tip for particular peer
     */
    case class GetCurrentTip(hostId: HostId) extends Message

    case class UpdatedReputation(
      performanceReputation:    Map[HostId, HostReputationValue],
      blockProvidingReputation: Map[HostId, HostReputationValue],
      noveltyReputation:        Map[HostId, Long]
    ) extends Message

    case object GetNetworkQualityForWarmHosts extends Message

    case object UpdateWarmHosts extends Message
  }

  // actor is option, because in future we shall be able to spawn actor without SetupPeer message,
  // thus actor could be shutdown in actor's non-active state
  case class Peer[F[_]: Logger](state: PeerState, actorOpt: Option[PeerActor[F]]) {

    def sendNoWait(message: PeerActor.Message): F[Unit] =
      actorOpt match {
        case Some(actor) => actor.sendNoWait(message)
        case None        => Logger[F].error(show"Try to send message to peer with no running client")
      }
  }

  case class State[F[_]](
    networkAlgebra:         NetworkAlgebra[F],
    reputationAggregator:   Option[ReputationAggregatorActor[F]],
    blocksChecker:          Option[BlockCheckerActor[F]], // TODO remove it
    requestsProxy:          Option[RequestsProxyActor[F]],
    peers:                  Map[HostId, Peer[F]],
    localChain:             LocalChainAlgebra[F],
    slotDataStore:          Store[F, BlockId, SlotData],
    transactionStore:       Store[F, TransactionId, IoTransaction],
    blockIdTree:            ParentChildTree[F, BlockId],
    headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F],
    p2pNetworkConfig:       P2PNetworkConfig
  )

  type Response[F[_]] = State[F]
  type PeersManagerActor[F[_]] = Actor[F, Message, Response[F]]

  def getFsm[F[_]: Async: Logger]: PeersManagerActor[F] => Fsm[F, State[F], Message, Response[F]] =
    (thisActor: PeersManagerActor[F]) =>
      Fsm {
        case (state, newPeer: SetupPeer[F] @unchecked)             => setupPeer(thisActor, state, newPeer)
        case (state, checker: SetupBlockChecker[F] @unchecked)     => setupBlockChecker(state, checker.blockChecker)
        case (state, checker: SetupRequestsProxy[F] @unchecked)    => setupRequestsProxy(state, checker.requestsProxy)
        case (state, agr: SetupReputationAggregator[F] @unchecked) => setupRepAggregator(state, agr.aggregator)
        case (state, update: UpdatePeerStatus)                     => updatePeerStatus(thisActor, state, update)
        case (state, BlockHeadersRequest(hostId, blocks))          => blockHeadersRequest(state, hostId, blocks)
        case (state, BlockBodyRequest(hostId, blockHeaders))       => blockDownloadRequest(state, hostId, blockHeaders)
        case (state, GetCurrentTips)                               => getCurrentTips(state)
        case (state, GetCurrentTip(hostId))                        => getCurrentTip(state, hostId)
        case (state, UpdatedReputation(perf, block, novelty))      => reputationUpdate(state, perf, block, novelty)
        case (state, GetNetworkQualityForWarmHosts)                => doNetworkQualityMeasure(state)
        case (state, UpdateWarmHosts)                              => updateWarmHosts(state)
      }

  def makeActor[F[_]: Async: Logger](
    networkAlgebra:         NetworkAlgebra[F],
    localChain:             LocalChainAlgebra[F],
    slotDataStore:          Store[F, BlockId, SlotData],
    transactionStore:       Store[F, TransactionId, IoTransaction],
    blockIdTree:            ParentChildTree[F, BlockId],
    headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F],
    p2pConfig:              P2PNetworkConfig
  ): Resource[F, PeersManagerActor[F]] = {
    val initialState =
      PeersManager.State[F](
        networkAlgebra,
        reputationAggregator = None,
        blocksChecker = None,
        requestsProxy = None,
        Map.empty[HostId, Peer[F]],
        localChain,
        slotDataStore,
        transactionStore,
        blockIdTree,
        headerToBodyValidation,
        p2pConfig
      )
    Actor.makeFull(initialState, getFsm[F], finalizeActor[F])
  }

  private def setupPeer[F[_]: Async: Logger](
    thisActor: PeersManagerActor[F],
    state:     State[F],
    setupPeer: SetupPeer[F]
  ): F[(State[F], Response[F])] = {
    val hostId: HostId = setupPeer.hostId
    val client: BlockchainPeerClient[F] = setupPeer.client

    require(state.blocksChecker.isDefined)
    require(state.reputationAggregator.isDefined)

    val peerActorF: F[PeerActor[F]] =
      thisActor.acquireActor(() =>
        PeerActor.makeActor(
          hostId,
          client,
          state.requestsProxy.get,
          state.reputationAggregator.get,
          state.localChain,
          state.slotDataStore,
          state.transactionStore,
          state.blockIdTree,
          state.headerToBodyValidation
        )
      )

    for {
      peer <- peerActorF.map(peerActor => Peer(PeerState.Hot, Option(peerActor)))
      newPeers = state.peers + (hostId -> peer)
      newState = state.copy(peers = newPeers)
    } yield (newState, newState)
  }

  private def setupBlockChecker[F[_]: Async: Logger](
    state:         State[F],
    blocksChecker: BlockCheckerActor[F]
  ): F[(State[F], Response[F])] = {
    val newState = state.copy(blocksChecker = Option(blocksChecker))
    Logger[F].info("Setup block checker for PeerManager") >>
    (newState, newState).pure[F]
  }

  private def setupRequestsProxy[F[_]: Async: Logger](
    state:         State[F],
    requestsProxy: RequestsProxyActor[F]
  ): F[(State[F], Response[F])] = {
    val newState = state.copy(requestsProxy = Option(requestsProxy))
    Logger[F].info("Setup requests proxy for PeerManager") >>
    (newState, newState).pure[F]
  }

  private def setupRepAggregator[F[_]: Async: Logger](
    state:      State[F],
    aggregator: ReputationAggregatorActor[F]
  ): F[(State[F], Response[F])] = {
    val newState = state.copy(reputationAggregator = Option(aggregator))
    Logger[F].info("Setup reputation aggregation") >>
    (newState, newState).pure[F]
  }

  private def updatePeerStatus[F[_]: Async](
    thisActor: PeersManagerActor[F],
    state:     State[F],
    newStatus: UpdatePeerStatus
  ): F[(State[F], Response[F])] = {
    val hostId: HostId = newStatus.hostId

    if (newStatus.newState != PeerState.Banned) {
      val peer: Peer[F] = state.peers.getOrElse(hostId, createNewPeer(state, hostId))
      val newPeers = state.peers + (hostId -> peer)
      val newState = state.copy(peers = newPeers)

      peer.sendNoWait(PeerActor.Message.UpdateState(newStatus.newState)) >>
      (newState, newState).pure[F]
    } else {
      val newState = state.copy(peers = state.peers - hostId)

      val releaseAction =
        for {
          peer      <- state.peers.get(hostId)
          peerActor <- peer.actorOpt
        } yield peerActor.sendNoWait(PeerActor.Message.UpdateState(PeerState.Banned)) >>
        thisActor.releaseActor(peerActor)

      releaseAction.getOrElse(().pure[F]) >>
      state.reputationAggregator
        .map(_.sendNoWait(ReputationAggregator.Message.StopTrackingReputationForHost(hostId)))
        .getOrElse(().pure[F]) >>
      (newState, newState).pure[F]
    }
  }

  // TODO Send NewRemoteHost to reputation aggregator
  private def createNewPeer[F[_]](state: State[F], hostId: HostId): Peer[F] = ???

  private def blockDownloadRequest[F[_]: Async](
    state:           State[F],
    requestedHostId: HostId,
    blocks:          NonEmptyChain[BlockHeader]
  ): F[(State[F], Response[F])] =
    state.peers.get(requestedHostId) match {
      case Some(peer) =>
        // TODO add logic if no peer actor is present
        peer.sendNoWait(PeerActor.Message.DownloadBlockBodies(blocks)) >>
        (state, state).pure[F]
      // TODO temporary solution, we shall check is block is available on other hosts first
      case None =>
        state.blocksChecker
          .map(_.sendNoWait(BlockChecker.Message.InvalidateBlockIds(blocks.map(_.id))))
          .getOrElse(().pure[F]) >>
        (state, state).pure[F]
    }

  private def blockHeadersRequest[F[_]: Async](
    state:           State[F],
    requestedHostId: HostId,
    blockIds:        NonEmptyChain[BlockId]
  ): F[(State[F], Response[F])] =
    state.peers.get(requestedHostId) match {
      case Some(peer) =>
        peer.sendNoWait(PeerActor.Message.DownloadBlockHeaders(blockIds)) >>
        (state, state).pure[F]
      // TODO temporary solution, we shall check is block is available on other hosts first
      case None =>
        state.blocksChecker
          .map(_.sendNoWait(BlockChecker.Message.InvalidateBlockIds(blockIds)))
          .getOrElse(().pure[F]) >>
        (state, state).pure[F]
    }

  private def getCurrentTips[F[_]: Async](state: State[F]): F[(State[F], Response[F])] =
    state.peers.values.filter(_.state.networkLevel).toSeq.traverse(_.sendNoWait(PeerActor.Message.GetCurrentTip)) >>
    (state, state).pure[F]

  private def getCurrentTip[F[_]: Async](state: State[F], hostId: HostId): F[(State[F], Response[F])] =
    state.peers
      .get(hostId)
      .filter(_.state.networkLevel)
      .map(_.sendNoWait(PeerActor.Message.GetCurrentTip))
      .getOrElse(().pure[F]) >>
    (state, state).pure[F]

  private def reputationUpdate[F[_]: Async: Logger](
    state:                    State[F],
    performanceReputation:    Map[HostId, HostReputationValue],
    blockProvidingReputation: Map[HostId, HostReputationValue],
    noveltyReputation:        Map[HostId, Long]
  ): F[(State[F], Response[F])] = {
    val p2pNetworkConfig = state.p2pNetworkConfig
    val currentHotPeers = getHotPeers(state)

    val hotToCold = getHostsToClose(
      currentHotPeers.keySet,
      p2pNetworkConfig,
      performanceReputation,
      blockProvidingReputation,
      noveltyReputation
    )
    val remainingHotConnections = currentHotPeers -- hotToCold

    val toOpenCount =
      Math.max(0, p2pNetworkConfig.networkProperties.minimumHotConnections - remainingHotConnections.size)
    val warmToHot = getNewHotConnectionHosts(state, performanceReputation, toOpenCount)

    // TODO warmToCold
    // TODO coldToWarm

    Logger[F].debug(
      show"Got update: Performance: " +
      show"${performanceReputation}; " +
      show"Block: ${blockProvidingReputation}; " +
      show"Novelty: ${noveltyReputation}"
    ) >>
    doHotToCold(state, hotToCold) >>
    doWarmToCold(state, warmToHot) >>
    (state, state).pure[F]
  }

  private def getHostsToClose[F[_]: Async](
    currentHotPeers:          Set[HostId],
    p2pNetworkConfig:         P2PNetworkConfig,
    performanceReputation:    Map[HostId, HostReputationValue],
    blockProvidingReputation: Map[HostId, HostReputationValue],
    noveltyReputation:        Map[HostId, Long]
  ): Set[HostId] = {
    val saveByNovelty =
      noveltyReputation.filter(_._2 > 0).keys.filter(currentHotPeers.contains).toSet

    val saveByBlockProviding =
      blockProvidingReputation.toSeq
        .filter { case (host, _) => currentHotPeers.contains(host) }
        .sortBy(_._2)
        .takeRight(p2pNetworkConfig.networkProperties.minimumBlockProvidingReputationPeers)
        .map(_._1)
        .toSet

    // TODO adjust performance reputation to avoid remote peer with best reputation but without actual application data providing
    val saveByPerformanceReputation =
      performanceReputation.toSeq
        .filter { case (host, _) => currentHotPeers.contains(host) }
        .sortBy(_._2)
        .takeRight(p2pNetworkConfig.networkProperties.minimumBlockProvidingReputationPeers)
        .map(_._1)
        .toSet

    val saveByOverallReputation =
      currentHotPeers.filter { host =>
        val performanceRep = performanceReputation.getOrElse(host, 0.0)
        val blockProvidingRep = blockProvidingReputation.getOrElse(host, 0.0)

        (performanceRep + blockProvidingRep) / 2 >= p2pNetworkConfig.networkProperties.minimumRequiredReputation
      }

    val allKeptConnections =
      saveByNovelty ++ saveByBlockProviding ++ saveByPerformanceReputation ++ saveByOverallReputation

    currentHotPeers -- allKeptConnections
  }

  private def doHotToCold[F[_]: Async](state: State[F], hostsToClose: Set[HostId]): F[Unit] = ().pure[F]

  private def getNewHotConnectionHosts[F[_]: Async](
    state:                 State[F],
    performanceReputation: Map[HostId, HostReputationValue],
    countToOpen:           Int
  ): Set[HostId] =
    if (countToOpen > 0) {
      val currentWarmPeers = getWarmPeers(state)

      performanceReputation.toSeq
        .filter { case (host, _) => currentWarmPeers.contains(host) }
        .sortBy(_._2)
        .takeRight(countToOpen)
        .map(_._1)
        .toSet
    } else {
      Set.empty
    }

  private def doWarmToCold[F[_]: Async](state: State[F], toOpen: Set[HostId]): F[Unit] =
    ().pure[F]

  private def doNetworkQualityMeasure[F[_]: Async](state: State[F]): F[(State[F], Response[F])] =
    getWarmPeers(state).values.toList.traverse(_.sendNoWait(PeerActor.Message.GetNetworkQuality)) >>
    (state, state).pure[F]

  private def updateWarmHosts[F[_]: Async](state: State[F]): F[(State[F], Response[F])] = (state, state).pure[F]

  private def finalizeActor[F[_]: Applicative](currentState: State[F]): F[Unit] = ().pure[F]

  private def getHotPeers[F[_]: Async](state: State[F]): Map[HostId, Peer[F]] = getPeers(state, PeerState.Hot)

  private def getWarmPeers[F[_]: Async](state: State[F]): Map[HostId, Peer[F]] = getPeers(state, PeerState.Warm)

  private def getPeers[F[_]: Async](state: State[F], peerState: PeerState): Map[HostId, Peer[F]] =
    state.peers.filter { case (_: HostId, peer) => peer.state == peerState }

}
