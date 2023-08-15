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
import scala.annotation.{nowarn, tailrec}

/**
 * Actor for managing peers
 */
object PeersManager {
  sealed trait Message

  object Message {

    /**
     * Setup appropriate actor for connection to peer specified by hostId, client is created outside before
     * @param hostId host to connect
     * @param client client with already opened connection to host
     * @tparam F effect
     */
    case class OpenedPeerConnection[F[_]](hostId: HostId, client: BlockchainPeerClient[F]) extends Message

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
     * Ban peer by some reason like invalid block id
     * @param hostId peer to be affected
     */
    case class BanPeer(hostId: HostId) extends Message

    /**
     * Peer connection had been closed
     *
     * @param hostId closed peer
     */
    case class ClosePeer(hostId: HostId) extends Message

    /**
     * Add known peers to peers list
     *
     * @param knownPeers peers to add
     */
    case class AddKnownPeers(knownPeers: NonEmptyChain[HostId]) extends Message

    /**
     * Add preWarm peers.
     * @param preWarmPeers peers to add
     */
    case class AddPreWarmPeers(preWarmPeers: NonEmptyChain[HostId]) extends Message

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

    /**
     * Reputation had been updated notification
     * @param performanceReputation updated performance reputation map
     * @param blockProvidingReputation updated block providing reputation map
     * @param noveltyReputation updated novelty reputation map
     */
    case class UpdatedReputation(
      performanceReputation:    Map[HostId, HostReputationValue],
      blockProvidingReputation: Map[HostId, HostReputationValue],
      noveltyReputation:        Map[HostId, Long]
    ) extends Message

    /**
     * Request measuring network quality from remote warm hosts
     */
    case object GetNetworkQualityForWarmHosts extends Message

    /**
     * Update warm hosts list
     */
    case class UpdateWarmHosts(performanceReputation: Map[HostId, HostReputationValue]) extends Message
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
    newPeerCreationAlgebra: PeerCreationRequestAlgebra[F],
    p2pNetworkConfig:       P2PNetworkConfig,
    coldToWarmSelector:     ColdToWarmSelector
  )

  type Response[F[_]] = State[F]
  type PeersManagerActor[F[_]] = Actor[F, Message, Response[F]]

  def getFsm[F[_]: Async: Logger]: PeersManagerActor[F] => Fsm[F, State[F], Message, Response[F]] =
    (thisActor: PeersManagerActor[F]) =>
      Fsm {
        case (state, checker: SetupBlockChecker[F] @unchecked)     => setupBlockChecker(state, checker.blockChecker)
        case (state, checker: SetupRequestsProxy[F] @unchecked)    => setupRequestsProxy(state, checker.requestsProxy)
        case (state, agr: SetupReputationAggregator[F] @unchecked) => setupRepAggregator(state, agr.aggregator)
        case (state, BlockHeadersRequest(hostId, blocks))          => blockHeadersRequest(state, hostId, blocks)
        case (state, BlockBodyRequest(hostId, blockHeaders))       => blockDownloadRequest(state, hostId, blockHeaders)
        case (state, GetNetworkQualityForWarmHosts)                => doNetworkQualityMeasure(state)
        case (state, GetCurrentTips)                               => getCurrentTips(state)
        case (state, GetCurrentTip(hostId))                        => getCurrentTip(state, hostId)
        case (state, ClosePeer(peer))                              => closePeer(thisActor, state, peer, PeerState.Cold)
        case (state, BanPeer(hostId))                              => banPeer(thisActor, state, hostId)
        case (state, newPeer: OpenedPeerConnection[F] @unchecked)  => openedPeerConnection(thisActor, state, newPeer)
        case (state, AddKnownPeers(peers))                         => addKnownPeers(state, peers)
        case (state, AddPreWarmPeers(peers))                       => addPreWarmPeers(state, peers)
        case (state, UpdateWarmHosts(performanceReputation)) => updateWarmHosts(thisActor, state, performanceReputation)
        case (state, UpdatedReputation(perf, block, novelty)) => repUpdate(thisActor, state, perf, block, novelty)
      }

  // TODO Add hostId for himself to avoid add himself as "known peer"
  def makeActor[F[_]: Async: Logger](
    networkAlgebra:         NetworkAlgebra[F],
    localChain:             LocalChainAlgebra[F],
    slotDataStore:          Store[F, BlockId, SlotData],
    transactionStore:       Store[F, TransactionId, IoTransaction],
    blockIdTree:            ParentChildTree[F, BlockId],
    headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F],
    newPeerCreationAlgebra: PeerCreationRequestAlgebra[F],
    p2pConfig:              P2PNetworkConfig,
    coldToWarmSelector:     ColdToWarmSelector = RandomColdToWarmSelector,
    initialPeers:           Map[HostId, Peer[F]] = Map.empty[HostId, Peer[F]]
  ): Resource[F, PeersManagerActor[F]] = {
    val initialState =
      PeersManager.State[F](
        networkAlgebra,
        reputationAggregator = None,
        blocksChecker = None,
        requestsProxy = None,
        initialPeers,
        localChain,
        slotDataStore,
        transactionStore,
        blockIdTree,
        headerToBodyValidation,
        newPeerCreationAlgebra,
        p2pConfig,
        coldToWarmSelector
      )

    Actor.makeFull(initialState, getFsm[F], finalizeActor[F])
  }

  private def getHotPeers[F[_]](state: State[F]): Map[HostId, Peer[F]] = getPeers(state, PeerState.Hot)

  private def getWarmPeers[F[_]](state: State[F]): Map[HostId, Peer[F]] = getPeers(state, PeerState.Warm)

  private def getColdPeers[F[_]](state: State[F]): Map[HostId, Peer[F]] = getPeers(state, PeerState.Cold)

  private def getPeers[F[_]](state: State[F], peerState: PeerState): Map[HostId, Peer[F]] =
    state.peers.filter { case (_: HostId, peer) => peer.state == peerState }

  private def getActivePeers[F[_]](state: State[F]): Map[HostId, Peer[F]] =
    state.peers.filter { case (_: HostId, peer) =>
      peer.state == PeerState.Hot || peer.state == PeerState.Warm || peer.state == PeerState.PreWarm
    }

  private def finalizeActor[F[_]: Applicative](@nowarn unusedCurrentState: State[F]): F[Unit] = Applicative[F].unit

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

  private def doNetworkQualityMeasure[F[_]: Async](state: State[F]): F[(State[F], Response[F])] =
    getWarmPeers(state).values.toList.traverse(_.sendNoWait(PeerActor.Message.GetNetworkQuality)) >>
    (state, state).pure[F]

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

  private def closePeer[F[_]: Async: Logger](
    thisActor:   PeersManagerActor[F],
    state:       State[F],
    hostId:      HostId,
    closeStatus: PeerState
  ): F[(State[F], Response[F])] = {
    def updateState(stateToUpdate: State[F]): F[State[F]] =
      stateToUpdate.peers
        .get(hostId)
        .map { peer =>
          // TODO we shall close actual connection as well
          peer.sendNoWait(PeerActor.Message.UpdateState(networkLevel = false, applicationLevel = false)) >>
          peer.actorOpt.map(thisActor.releaseActor).getOrElse(().pure[F]) >>
          stateToUpdate.copy(peers = stateToUpdate.peers + (hostId -> Peer(closeStatus, None))).pure[F]
        }
        .getOrElse(Logger[F].error("Try to close non exist peer $peer") >> stateToUpdate.pure[F])

    def notifyReputationAggregator(currentState: State[F]): F[Unit] =
      currentState.reputationAggregator
        .map(_.sendNoWait(ReputationAggregator.Message.PeerIsCold(hostId)))
        .getOrElse(().pure[F])

    for {
      _        <- Logger[F].info(show"Going to close peer $hostId")
      newState <- updateState(state)
      _        <- notifyReputationAggregator(newState)
    } yield (newState, newState)
  }

  private def banPeer[F[_]: Async: Logger](
    thisActor: PeersManagerActor[F],
    state:     State[F],
    hostId:    HostId
  ): F[(State[F], Response[F])] = closePeer(thisActor, state, hostId, PeerState.Banned)

  private def openedPeerConnection[F[_]: Async: Logger](
    thisActor: PeersManagerActor[F],
    state:     State[F],
    setupPeer: OpenedPeerConnection[F]
  ): F[(State[F], Response[F])] = {
    val hostId: HostId = setupPeer.hostId
    val client: BlockchainPeerClient[F] = setupPeer.client

    require(state.requestsProxy.isDefined)
    require(state.reputationAggregator.isDefined)
    require(state.blocksChecker.isDefined)

    val peerActorF: F[PeerActor[F]] =
      thisActor.acquireActor(() =>
        state.networkAlgebra.makePeer(
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

    if (state.peers.get(hostId).flatMap(_.actorOpt).isDefined) {
      Logger[F].error(show"Try to redefine actor for remote peer $hostId") >>
      (state, state).pure[F]
    } else {
      for {
        _    <- Logger[F].info(show"New connection to remote peer $hostId had been established")
        peer <- peerActorF.map(peerActor => Peer(PeerState.Warm, Option(peerActor)))
        _    <- peer.sendNoWait(PeerActor.Message.GetNetworkQuality)
        _    <- peer.sendNoWait(PeerActor.Message.UpdateState(networkLevel = true, applicationLevel = false))
        newPeers = state.peers + (hostId -> peer)
        newState = state.copy(peers = newPeers)
      } yield (newState, newState)
    }
  }

  private def addKnownPeers[F[_]: Async: Logger](
    state:      State[F],
    knownPeers: NonEmptyChain[HostId]
  ): F[(State[F], Response[F])] = {
    val peersToAdd =
      knownPeers.filterNot(state.peers.contains).map(h => h -> Peer(PeerState.Cold, None)).toList
    val newPeers = state.peers ++ peersToAdd
    val newState = state.copy(peers = newPeers)

    Logger[F].info(show"Added know peers: $knownPeers") >>
    (newState, newState).pure[F]
  }

  private def addPreWarmPeers[F[_]: Async: Logger](
    state:        State[F],
    preWarmPeers: NonEmptyChain[HostId]
  ): F[(State[F], Response[F])] = {
    val peersToAdd = preWarmPeers.toList.toSet -- getActivePeers(state).keySet
    val newPeers = state.peers ++ peersToAdd.map(host => host -> Peer(PeerState.PreWarm, None))
    val newState = state.copy(peers = newPeers)

    peersToAdd.toList.traverse(state.newPeerCreationAlgebra.requestNewPeerCreation) >>
    (newState, newState).pure[F]
  }

  private def updateWarmHosts[F[_]: Async: Logger](
    thisActor:             PeersManagerActor[F],
    state:                 State[F],
    performanceReputation: Map[HostId, HostReputationValue]
  ): F[(State[F], Response[F])] = {
    val warmHostsByReputation =
      getWarmPeers(state)
        .map { case (host, _) => (host, performanceReputation.getOrElse(host, 0.0)) }
        .toList
        .sortBy(_._2)
    val warmHostsSize = warmHostsByReputation.size
    val warmToCold = warmHostsByReputation.take(warmHostsSize / 2).map(_._1)

    for {
      newState <- peersToCold(thisActor, state, warmToCold.toSet)
      _ <- getHotPeers(state).values
        .flatMap(_.actorOpt)
        .toList
        .traverse(_.sendNoWait(PeerActor.Message.GetHotPeersFromPeer))
    } yield (newState, newState)
  }

  private def repUpdate[F[_]: Async: Logger](
    thisActor:  PeersManagerActor[F],
    state:      State[F],
    perfRep:    Map[HostId, HostReputationValue],
    blockRep:   Map[HostId, HostReputationValue],
    noveltyRep: Map[HostId, Long]
  ): F[(State[F], Response[F])] = {
    val perfRepDefault: Map[HostId, HostReputationValue] = perfRep.withDefaultValue(0.0)
    val blockRepDefault: Map[HostId, HostReputationValue] = blockRep.withDefaultValue(0.0)
    val noveltyRepDefault: Map[HostId, Long] = noveltyRep.withDefaultValue(0L)

    for {
      _ <- Logger[F].trace(s"Peers: ${state.peers.view.mapValues(_.state).toMap}")
      _ <- Logger[F].debug(show"Got update: Performance: $perfRep; Block: $blockRep; Novelty: $noveltyRep")

      stateWithPreWarm     <- coldToPreWarm(state)
      stateWithClosedPeers <- hotToCold(thisActor, stateWithPreWarm, perfRepDefault, blockRepDefault, noveltyRepDefault)
      stateWithNewHotPeers <- warmToHot(stateWithClosedPeers, perfRepDefault)
    } yield (stateWithNewHotPeers, stateWithNewHotPeers)
  }

  private def hotToCold[F[_]: Async: Logger](
    thisActor:  PeersManagerActor[F],
    state:      State[F],
    perfRep:    Map[HostId, HostReputationValue],
    blockRep:   Map[HostId, HostReputationValue],
    noveltyRep: Map[HostId, Long]
  ): F[State[F]] = {
    val currentHotPeers = getHotPeers(state).keySet
    val hotToCold = getHostsToClose(currentHotPeers, state.p2pNetworkConfig, perfRep, blockRep, noveltyRep)

    peersToCold(thisActor, state, hotToCold)
  }

  private def warmToHot[F[_]: Async: Logger](
    state:   State[F],
    perfRep: Map[HostId, HostReputationValue]
  ): F[State[F]] = {
    val minimumHotConnections = state.p2pNetworkConfig.networkProperties.minimumHotConnections
    val lackHotPeersCount = minimumHotConnections - getHotPeers(state).size
    val warmToHot = getNewHotConnectionHosts(state, perfRep, lackHotPeersCount)

    warmPeersToHot(state, warmToHot)
  }

  private def getNewHotConnectionHosts[F[_]](
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

  private def coldToPreWarm[F[_]: Async: Logger](state: State[F]): F[State[F]] = {
    val minimumWarmConnection = state.p2pNetworkConfig.networkProperties.minimumWarmConnections
    val lackWarmPeersCount = minimumWarmConnection - getWarmPeers(state).size
    val coldToPreWarm = state.coldToWarmSelector.select(getColdPeers(state).keySet, lackWarmPeersCount)

    peersToPreWarm(state, coldToPreWarm)
  }

  private def getHostsToClose(
    currentHotPeers:          Set[HostId],
    p2pNetworkConfig:         P2PNetworkConfig,
    performanceReputation:    Map[HostId, HostReputationValue],
    blockProvidingReputation: Map[HostId, HostReputationValue],
    noveltyReputation:        Map[HostId, Long]
  ): Set[HostId] = {

    // TODO adjust performance reputation to avoid remote peer with best reputation but
    //  without actual application data providing
    val saveByPerformanceReputation =
      currentHotPeers.toSeq
        .map(h => h -> performanceReputation(h))
        .sortBy(_._2)
        .takeRight(p2pNetworkConfig.networkProperties.minimumPerformanceReputationPeers)
        .map(_._1)
        .toSet

    val saveByBlockProviding =
      currentHotPeers.toSeq
        .map(h => h -> blockProvidingReputation(h))
        .sortBy(_._2)
        .takeRight(p2pNetworkConfig.networkProperties.minimumBlockProvidingReputationPeers)
        .map(_._1)
        .toSet

    val saveByNovelty = currentHotPeers.filter(noveltyReputation(_) > 0)

    val saveByOverallReputation =
      currentHotPeers.filter { host =>
        val performanceRep = performanceReputation(host)
        val blockProvidingRep = blockProvidingReputation(host)

        (performanceRep + blockProvidingRep) / 2 >= p2pNetworkConfig.networkProperties.minimumRequiredReputation
      }

    val allKeptConnections =
      saveByNovelty ++ saveByBlockProviding ++ saveByPerformanceReputation ++ saveByOverallReputation

    currentHotPeers -- allKeptConnections
  }

  private def peersToCold[F[_]: Async: Logger](
    thisActor:    PeersManagerActor[F],
    state:        State[F],
    hostsToClose: Set[HostId]
  ): F[State[F]] = {
    @tailrec
    def iteration(acc: State[F] => F[State[F]], hostsToClose: List[HostId]): State[F] => F[State[F]] =
      hostsToClose match {
        case head :: tail =>
          val closePeerFun: State[F] => F[State[F]] = closePeer(thisActor, _, head, PeerState.Cold).map(_._1)
          val newAcc: State[F] => F[State[F]] = (a: State[F]) => acc(a).flatMap(closePeerFun)
          iteration(newAcc, tail)
        case Nil => acc
      }

    Logger[F].infoIf(hostsToClose.nonEmpty, show"Going to cold next hosts: $hostsToClose") >>
    iteration(state => state.pure[F], hostsToClose.toList).apply(state)
  }

  private def warmPeersToHot[F[_]: Async: Logger](state: State[F], toHot: Set[HostId]): F[State[F]] = {
    val peerUpdateMessagesF = toHot.toList.traverse(host =>
      state.peers
        .get(host)
        .flatMap(_.actorOpt)
        .map(_.sendNoWait(PeerActor.Message.UpdateState(networkLevel = true, applicationLevel = true)))
        .getOrElse(Logger[F].error("Try to move peer without actor to hot state"))
    )

    val sendReputationUpdateF = {
      for {
        reputationAggregator <- state.reputationAggregator
        message              <- NonEmptyChain.fromSeq(toHot.toSeq).map(ReputationAggregator.Message.NewHotPeer)
      } yield reputationAggregator.sendNoWait(message)
    }.getOrElse(().pure[F])

    val newPeers =
      state.peers.view.filterKeys(toHot.contains).mapValues(_.copy(state = PeerState.Hot)).toMap

    Logger[F].infoIf(toHot.nonEmpty, show"Going to hot next hosts: $toHot") >>
    peerUpdateMessagesF >>
    sendReputationUpdateF >>
    state.copy(peers = state.peers ++ newPeers).pure[F]
  }

  private def peersToPreWarm[F[_]: Async: Logger](state: State[F], peers: Set[HostId]): F[State[F]] =
    NonEmptyChain
      .fromSeq(peers.toSeq)
      .map(peers =>
        Logger[F].info(show"Going to warm next hosts: $peers") >>
        addPreWarmPeers(state, peers).map(_._1)
      )
      .getOrElse(state.pure[F])

}
