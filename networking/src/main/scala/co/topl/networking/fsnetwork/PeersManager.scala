package co.topl.networking.fsnetwork

import cats.Applicative
import cats.data.NonEmptyChain
import cats.effect.{Async, Resource}
import cats.implicits._
import co.topl.actor.{Actor, Fsm}
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
import co.topl.networking.p2p.RemoteAddress
import com.github.benmanes.caffeine.cache.Cache
import org.typelevel.log4cats.Logger

import scala.annotation.tailrec

/**
 * Actor for managing peers
 */
object PeersManager {
  sealed trait Message

  object Message {

    /**
     * Setup appropriate actor for connection to peer specified by hostId, client is created outside before
     * @param address host to connect
     * @param client client with already opened connection to host
     * @tparam F effect
     */
    case class OpenedPeerConnection[F[_]](address: RemoteAddress, client: BlockchainPeerClient[F]) extends Message

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
    case class AddKnownPeers(knownPeers: NonEmptyChain[RemoteAddress]) extends Message

    /**
     * Add preWarm peers.
     * @param preWarmPeers peers to add
     */
    case class AddPreWarmPeers(preWarmPeers: NonEmptyChain[RemoteAddress]) extends Message

    /**
     * @param hostId use hostId as a possible hint
     * @param blockIds list of block's id of headers to be requested from peer
     */
    case class BlockHeadersRequest(hostId: Option[HostId], blockIds: NonEmptyChain[BlockId]) extends Message

    /**
     * @param hostId use hostId as a possible hint
     * @param blockHeaders requested bodies
     */
    case class BlockBodyRequest(hostId: Option[HostId], blockHeaders: NonEmptyChain[BlockHeader]) extends Message

    /**
     * Update block availability on remote peer
     * @param sources block source
     */
    case class BlocksSource(sources: NonEmptyChain[(HostId, BlockId)]) extends Message

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

    /**
     * Add remote peer server
     */
    case class RemotePeerServerPort(hostId: HostId, remotePeerServerPort: Int) extends Message
  }

  case class State[F[_]](
    thisHostId:             HostId,
    networkAlgebra:         NetworkAlgebra[F],
    reputationAggregator:   Option[ReputationAggregatorActor[F]],
    blocksChecker:          Option[BlockCheckerActor[F]], // TODO remove it
    requestsProxy:          Option[RequestsProxyActor[F]],
    peers:                  PeersHandler[F],
    localChain:             LocalChainAlgebra[F],
    slotDataStore:          Store[F, BlockId, SlotData],
    transactionStore:       Store[F, TransactionId, IoTransaction],
    blockIdTree:            ParentChildTree[F, BlockId],
    headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F],
    newPeerCreationAlgebra: PeerCreationRequestAlgebra[F],
    p2pNetworkConfig:       P2PNetworkConfig,
    coldToWarmSelector:     ColdToWarmSelector[F],
    hotPeersUpdate:         Set[RemoteAddress] => F[Unit],
    blockSource:            Cache[BlockId, Set[HostId]]
  )

  type Response[F[_]] = State[F]
  type PeersManagerActor[F[_]] = Actor[F, Message, Response[F]]

  def getFsm[F[_]: Async: Logger: DnsResolver]: PeersManagerActor[F] => Fsm[F, State[F], Message, Response[F]] =
    (thisActor: PeersManagerActor[F]) =>
      Fsm {
        case (state, checker: SetupBlockChecker[F] @unchecked)     => setupBlockChecker(state, checker.blockChecker)
        case (state, checker: SetupRequestsProxy[F] @unchecked)    => setupRequestsProxy(state, checker.requestsProxy)
        case (state, agr: SetupReputationAggregator[F] @unchecked) => setupRepAggregator(state, agr.aggregator)
        case (state, BlockHeadersRequest(hostId, blocks))          => blockHeadersRequest(state, hostId, blocks)
        case (state, BlockBodyRequest(hostId, blockHeaders))       => blockDownloadRequest(state, hostId, blockHeaders)
        case (state, BlocksSource(sources))                        => blocksSourceProcessing(state, sources)
        case (state, GetNetworkQualityForWarmHosts)                => doNetworkQualityMeasure(state)
        case (state, GetCurrentTips)                               => getCurrentTips(state)
        case (state, GetCurrentTip(hostId))                        => getCurrentTip(state, hostId)
        case (state, RemotePeerServerPort(peer, peerServerPort))   => remotePeerServerPort(state, peer, peerServerPort)
        case (state, ClosePeer(peer))                              => closePeer(thisActor, state, peer, PeerState.Cold)
        case (state, BanPeer(hostId))                              => banPeer(thisActor, state, hostId)
        case (state, newPeer: OpenedPeerConnection[F] @unchecked)  => openedPeerConnection(thisActor, state, newPeer)
        case (state, AddKnownPeers(peers))                         => addKnownPeers(state, peers)
        case (state, AddPreWarmPeers(peers))                       => addPreWarmPeers(state, peers)
        case (state, UpdateWarmHosts(perfReputation))              => updateWarmHosts(thisActor, state, perfReputation)
        case (state, UpdatedReputation(perf, block, novelty))      => repUpdate(thisActor, state, perf, block, novelty)
      }

  def makeActor[F[_]: Async: Logger: DnsResolver](
    thisHostId:             HostId,
    networkAlgebra:         NetworkAlgebra[F],
    localChain:             LocalChainAlgebra[F],
    slotDataStore:          Store[F, BlockId, SlotData],
    transactionStore:       Store[F, TransactionId, IoTransaction],
    blockIdTree:            ParentChildTree[F, BlockId],
    headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F],
    newPeerCreationAlgebra: PeerCreationRequestAlgebra[F],
    p2pConfig:              P2PNetworkConfig,
    hotPeersUpdate:         Set[RemoteAddress] => F[Unit],
    savePeersFunction:      Set[RemoteAddress] => F[Unit],
    coldToWarmSelector:     ColdToWarmSelector[F],
    initialPeers:           Map[HostId, Peer[F]],
    blockSource:            Cache[BlockId, Set[HostId]]
  ): Resource[F, PeersManagerActor[F]] =
    for {
      resolvedHost <- Resource.liftK(thisHostId.resolveHost.map(_.get)) // TODO error processing
      initialState =
        PeersManager.State[F](
          resolvedHost,
          networkAlgebra,
          reputationAggregator = None,
          blocksChecker = None,
          requestsProxy = None,
          PeersHandler(initialPeers),
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          headerToBodyValidation,
          newPeerCreationAlgebra,
          p2pConfig,
          coldToWarmSelector,
          hotPeersUpdate,
          blockSource
        )
      actorName = s"Peers manager actor"
      _     <- Resource.liftK(Logger[F].info(s"Start PeerManager for host ${initialState.thisHostId}"))
      actor <- Actor.makeFull(actorName, initialState, getFsm[F], finalizeActor[F](savePeersFunction))
    } yield actor

  private def finalizeActor[F[_]](
    savePeersFunction: Set[RemoteAddress] => F[Unit]
  )(state: State[F]): F[Unit] =
    savePeersFunction(state.peers.getAvailableToConnectAddresses)

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
    state:    State[F],
    hostId:   Option[HostId],
    blockIds: NonEmptyChain[BlockId]
  ): F[(State[F], Response[F])] =
    // request is done for linked blocks, i.e. last block is child for any other block in request,
    // thus we could use it for detect block source. TODO make request parallel
    getHotPeerByBlockId(state, blockIds.last, hostId) match {
      case Some(peer) =>
        peer.sendNoWait(PeerActor.Message.DownloadBlockHeaders(blockIds)) >>
        (state, state).pure[F]
      case None =>
        state.blocksChecker
          .map(_.sendNoWait(BlockChecker.Message.InvalidateBlockIds(NonEmptyChain.one(blockIds.last))))
          .getOrElse(().pure[F]) >>
        (state, state).pure[F]
    }

  private def blockDownloadRequest[F[_]: Async](
    state:  State[F],
    hostId: Option[HostId],
    blocks: NonEmptyChain[BlockHeader]
  ): F[(State[F], Response[F])] =
    // request is done for linked blocks, i.e. last block is child for any other block in request,
    // thus we could use it for detect block source. TODO make request parallel
    getHotPeerByBlockId(state, blocks.last.id, hostId) match {
      case Some(peer) =>
        peer.sendNoWait(PeerActor.Message.DownloadBlockBodies(blocks)) >>
        (state, state).pure[F]
      case None =>
        state.blocksChecker
          .map(_.sendNoWait(BlockChecker.Message.InvalidateBlockIds(NonEmptyChain.one(blocks.last.id))))
          .getOrElse(().pure[F]) >>
        (state, state).pure[F]
    }

  private def getHotPeerByBlockId[F[_]](state: State[F], blockId: BlockId, hostId: Option[HostId]): Option[Peer[F]] =
    for {
      sources <- Option(state.blockSource.getOrElse(blockId, Set.empty) ++ hostId.toSet)
      source  <- state.peers.getHotPeers.keySet.find(sources.contains)
      peer    <- state.peers.get(source)
    } yield peer

  private def blocksSourceProcessing[F[_]: Async](
    state:   State[F],
    sources: NonEmptyChain[(HostId, BlockId)]
  ): F[(State[F], Response[F])] = {
    val toSend: Seq[(HostId, Long)] =
      sources.toList.flatMap { case (host, blockId) =>
        val previousSource: Set[HostId] = state.blockSource.get(blockId).getOrElse(Set.empty[HostId])
        if (previousSource.contains(host)) {
          Option.empty[(HostId, Long)]
        } else {
          val newSource: Set[HostId] = previousSource + host
          state.blockSource.put(blockId, newSource)
          Option((host, newSource.size))
        }
      }

    val sendMessage: NonEmptyChain[(HostId, Long)] => F[Unit] =
      d =>
        state.reputationAggregator
          .map(_.sendNoWait(ReputationAggregator.Message.BlockProvidingReputationUpdate(d)))
          .getOrElse(Applicative[F].unit)

    NonEmptyChain.fromSeq(toSend).map(sendMessage).getOrElse(().pure[F]) >> (state, state).pure[F]
  }

  private def doNetworkQualityMeasure[F[_]: Async](state: State[F]): F[(State[F], Response[F])] =
    state.peers.getWarmPeers.values.toSeq.traverse(_.sendNoWait(PeerActor.Message.GetNetworkQuality)) >>
    (state, state).pure[F]

  private def getCurrentTips[F[_]: Async](state: State[F]): F[(State[F], Response[F])] =
    state.peers.getHotPeers.values.toSeq.traverse(_.sendNoWait(PeerActor.Message.GetCurrentTip)) >>
    (state, state).pure[F]

  private def getCurrentTip[F[_]: Async](state: State[F], hostId: HostId): F[(State[F], Response[F])] =
    state.peers
      .get(hostId)
      .filter(_.state.applicationLevel)
      .map(_.sendNoWait(PeerActor.Message.GetCurrentTip))
      .getOrElse(().pure[F]) >>
    (state, state).pure[F]

  private def remotePeerServerPort[F[_]: Async: Logger](
    state:      State[F],
    hostId:     HostId,
    serverPort: Int
  ): F[(State[F], Response[F])] =
    for {
      _        <- Logger[F].info(s"For peer $hostId received server address: $serverPort")
      newState <- state.copy(peers = state.peers.copyWithUpdatedServerPort(hostId, serverPort)).pure[F]
      _        <- updateExternalHotPeersList(newState)
    } yield (newState, newState)

  private def updateExternalHotPeersList[F[_]: Async: Logger](state: State[F]): F[Unit] =
    for {
      hotPeersServers <- state.peers.getPeersWithPort(PeerState.Hot).map(_.asRemoteAddress).pure[F]
      _               <- Logger[F].debug(s"Going to update hot peers servers $hotPeersServers")
      _               <- state.hotPeersUpdate(hotPeersServers)
    } yield ()

  private def closePeer[F[_]: Async: Logger](
    thisActor:   PeersManagerActor[F],
    state:       State[F],
    hostId:      HostId,
    closeStatus: PeerState
  ): F[(State[F], Response[F])] = {

    def notifyActors(hostId: HostId, peer: Peer[F]): F[Unit] =
      // TODO we shall close actual connection as well
      peer.sendNoWait(PeerActor.Message.UpdateState(networkLevel = false, applicationLevel = false)) >>
      peer.actorOpt.map(a => thisActor.releaseActor(a).void).getOrElse(Applicative[F].unit) >>
      state.reputationAggregator
        .map(_.sendNoWait(ReputationAggregator.Message.PeerIsCold(hostId)))
        .getOrElse(().pure[F])

    val timeoutWindow = state.p2pNetworkConfig.networkProperties.closeTimeoutWindowInMs
    for {
      _ <- Logger[F].info(s"Going to close peer $hostId with peer ${state.peers.get(hostId)}")
      (closedPeers, newPeerHandler) <- state.peers
        .moveToClosedState(hostId, closeStatus, System.currentTimeMillis(), timeoutWindow)
        .pure[F]
      _        <- closedPeers.toSeq.traverse((notifyActors _).tupled)
      newState <- state.copy(peers = newPeerHandler).pure[F]
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
    val hostId: HostId = setupPeer.address.host
    val client: BlockchainPeerClient[F] = setupPeer.client

    require(state.requestsProxy.isDefined)
    require(state.reputationAggregator.isDefined)
    require(state.blocksChecker.isDefined)

    def setupPeerActor: F[PeerActor[F]] =
      thisActor
        .acquireActor(() =>
          state.networkAlgebra.makePeer(
            hostId,
            state.networkAlgebra,
            client,
            state.requestsProxy.get,
            state.reputationAggregator.get,
            thisActor,
            state.localChain,
            state.slotDataStore,
            state.transactionStore,
            state.blockIdTree,
            state.headerToBodyValidation
          )
        )
        .flatTap { peerActor =>
          peerActor.sendNoWait(PeerActor.Message.UpdateState(networkLevel = true, applicationLevel = false)) >>
          peerActor.sendNoWait(PeerActor.Message.GetNetworkQuality) >>
          peerActor.sendNoWait(PeerActor.Message.GetPeerServerAddress)
        }

    if (state.peers.get(hostId).flatMap(_.actorOpt).isDefined) {
      Logger[F].error(show"Try to redefine actor for remote peer $hostId") >>
      (state, state).pure[F]
    } else {
      for {
        _                     <- Logger[F].info(show"New connection to remote peer $hostId had been established")
        (warmPeers, withWarm) <- state.peers.copyWithAddedHost(hostId).moveToState(hostId, PeerState.Warm).pure[F]
        withActor <- warmPeers
          .contains(hostId)
          .pure[F]
          .ifM(
            ifTrue = Logger[F].info(s"Accept remote peer $hostId as new warm connection") >>
              setupPeerActor.map(peerActor => withWarm.copyWithNewPeerActor(hostId, peerActor)),
            ifFalse = Logger[F].info(s"Decline remote peer $hostId as new warm connection") >>
              withWarm.pure[F]
          )
        newState = state.copy(peers = withActor)
      } yield (newState, newState)
    }
  }

  private def resolveHosts[F[_]: Async: Logger: DnsResolver](
    unresolved: NonEmptyChain[RemoteAddress]
  ): F[Seq[RemoteAddress]] =
    unresolved.toList
      .traverse { unresolvedAddress =>
        unresolvedAddress.host.resolveHost
          .map(resolvedOpt => resolvedOpt.map(resolvedHost => RemoteAddress(resolvedHost, unresolvedAddress.port)))
          .flatTap(resolvedAddress => Logger[F].debug(s"Resolve address $unresolvedAddress to $resolvedAddress"))
      }
      .map(_.flatten.toSeq)

  private def addKnownPeers[F[_]: Async: Logger: DnsResolver](
    state:      State[F],
    knownPeers: NonEmptyChain[RemoteAddress]
  ): F[(State[F], Response[F])] =
    for {
      resolvedPeers       <- resolveHosts(knownPeers)
      oldPeers            <- state.peers.pure[F]
      newPeers            <- oldPeers.copyWithAddedRemoteAddresses(resolvedPeers.toSet).pure[F]
      peersHadBeenChanged <- (newPeers.peers.size != oldPeers.peers.size).pure[F]
      _                   <- Logger[F].infoIf(peersHadBeenChanged, s"Add some peers from: $knownPeers")
      newState            <- state.copy(peers = newPeers).pure[F]
    } yield (newState, newState)

  private def addPreWarmPeers[F[_]: Async: Logger: DnsResolver](
    state:        State[F],
    preWarmPeers: NonEmptyChain[RemoteAddress]
  ): F[(State[F], Response[F])] =
    for {
      _             <- Logger[F].trace(s"Receiving request to prewarm next peers: $preWarmPeers")
      resolvedPeers <- resolveHosts(preWarmPeers)
      addressToAdd  <- resolvedPeers.filter(_.host != state.thisHostId).toSet.pure[F]

      (preWarmedHosts, newPeers) <-
        state.peers
          .copyWithAddedRemoteAddresses(addressToAdd)
          .moveToState(addressToAdd.map(_.host), PeerState.PreWarm)
          .pure[F]

      addedAddresses <- addressToAdd.filter(ra => preWarmedHosts.contains(ra.host)).toSeq.pure[F]

      newState <- state.copy(peers = newPeers).pure[F]
      _        <- Logger[F].infoIf(addedAddresses.nonEmpty, s"Going to pre warm next peers: $addedAddresses")
      _        <- addedAddresses.traverse(state.newPeerCreationAlgebra.requestNewPeerCreation)
    } yield (newState, newState)

  private def updateWarmHosts[F[_]: Async: Logger](
    thisActor:             PeersManagerActor[F],
    state:                 State[F],
    performanceReputation: Map[HostId, HostReputationValue]
  ): F[(State[F], Response[F])] = {
    val minimumWarmPeers = state.p2pNetworkConfig.networkProperties.minimumWarmConnections
    val warmHostsByReputation =
      state.peers.getWarmPeers
        .map { case (host, _) => (host, performanceReputation.getOrElse(host, 0.0)) }
        .toList
        .sortBy(_._2)
    val warmHostsSize = warmHostsByReputation.size
    val warmToCold = warmHostsByReputation.take((warmHostsSize - minimumWarmPeers) / 2).map(_._1)

    for {
      _        <- Logger[F].infoIf(warmToCold.nonEmpty, s"Update warm hosts, set warm hosts $warmToCold to cold state")
      newState <- peersToCold(thisActor, state, warmToCold.toSet)
      _        <- requestNeighboursFromHotPeers(state)
    } yield (newState, newState)
  }

  private def requestNeighboursFromHotPeers[F[_]: Async: Logger](state: State[F]): F[Unit] = {
    val newWarmPeerCount = state.p2pNetworkConfig.networkProperties.minimumWarmConnections
    val maximumWarmConnections = state.p2pNetworkConfig.networkProperties.maximumWarmConnections
    val warmPeersSize = state.peers.getWarmPeers.size

    if (warmPeersSize < maximumWarmConnections && newWarmPeerCount > 0) {
      val currentHotPeers = state.peers.getHotPeers
      Logger[F].debug(s"Request neighbour(s) from peers: ${currentHotPeers.keySet}") >>
      currentHotPeers.values.toList
        .traverse(_.sendNoWait(PeerActor.Message.GetHotPeersFromPeer(newWarmPeerCount)))
        .void
    } else {
      Logger[F].info(s"Do not request neighbours, warmPeersSize: $warmPeersSize, newWarmPeerCount: $newWarmPeerCount")
    }
  }

  private def repUpdate[F[_]: Async: Logger: DnsResolver](
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
      _ <- Logger[F].debug(s"Peers: ${state.peers}")
      _ <- Logger[F].debug(show"Got update: Performance: $perfRep; Block: $blockRep; Novelty: $noveltyRep")

      stateWithPreWarm     <- coldToPreWarm(state)
      stateWithClosedPeers <- hotToCold(thisActor, stateWithPreWarm, perfRepDefault, blockRepDefault, noveltyRepDefault)
      stateWithNewHotPeers <- warmToHot(stateWithClosedPeers, perfRepDefault)
      _                    <- updateExternalHotPeersList(stateWithNewHotPeers)
    } yield (stateWithNewHotPeers, stateWithNewHotPeers)
  }

  private def hotToCold[F[_]: Async: Logger](
    thisActor:  PeersManagerActor[F],
    state:      State[F],
    perfRep:    Map[HostId, HostReputationValue],
    blockRep:   Map[HostId, HostReputationValue],
    noveltyRep: Map[HostId, Long]
  ): F[State[F]] = {
    val currentHotPeers = state.peers.getHotPeers.keySet
    val hotToCold = getHostsToClose(currentHotPeers, state.p2pNetworkConfig, perfRep, blockRep, noveltyRep)

    Logger[F].infoIf(
      hotToCold.nonEmpty,
      s"Going to close $hotToCold due of bad reputation. Reputations:$perfRep; $blockRep; $noveltyRep"
    ) >>
    peersToCold(thisActor, state, hotToCold)
  }

  private def warmToHot[F[_]: Async: Logger](
    state:   State[F],
    perfRep: Map[HostId, HostReputationValue]
  ): F[State[F]] = {
    val minimumHotConnections = state.p2pNetworkConfig.networkProperties.minimumHotConnections
    val lackHotPeersCount = minimumHotConnections - state.peers.getHotPeers.size
    val warmToHot = getNewHotConnectionHosts(state, perfRep, lackHotPeersCount)

    warmPeersToHot(state, warmToHot)
  }

  private def getNewHotConnectionHosts[F[_]](
    state:                 State[F],
    performanceReputation: Map[HostId, HostReputationValue],
    countToOpen:           Int
  ): Set[HostId] =
    if (countToOpen > 0) {
      val currentWarmPeers = state.peers.getWarmPeers

      performanceReputation.toSeq
        .filter { case (host, _) => currentWarmPeers.contains(host) }
        .sortBy(_._2)
        .takeRight(countToOpen)
        .map(_._1)
        .toSet
    } else {
      Set.empty
    }

  private def coldToPreWarm[F[_]: Async: Logger: DnsResolver](state: State[F]): F[State[F]] = {
    val minimumWarmConnection = state.p2pNetworkConfig.networkProperties.minimumWarmConnections
    val lackWarmPeersCount = minimumWarmConnection - state.peers.getWarmPeers.size
    val addressToOpen: Set[PeerWithHostAndPort[F]] = state.peers.getPeersWithPort(PeerState.Cold)
    val hostColdToPreWarm: Set[RemoteAddress] = state.coldToWarmSelector.select(addressToOpen, lackWarmPeersCount)

    NonEmptyChain
      .fromSeq(hostColdToPreWarm.toSeq)
      .map(peers => addPreWarmPeers(state, peers).map(_._1))
      .getOrElse(state.pure[F])
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
    def peerUpdateMessagesF(hotPeers: Set[HostId]) = hotPeers.toList.traverse(host =>
      state.peers
        .get(host)
        .flatMap(_.actorOpt)
        .map(_.sendNoWait(PeerActor.Message.UpdateState(networkLevel = true, applicationLevel = true)))
        .getOrElse(Logger[F].error("Try to move peer without actor to hot state"))
    )

    def sendReputationUpdateF(hotPeers: Set[HostId]) = {
      for {
        reputationAggregator <- state.reputationAggregator
        message              <- NonEmptyChain.fromSeq(hotPeers.toSeq).map(ReputationAggregator.Message.NewHotPeer)
      } yield reputationAggregator.sendNoWait(message)
    }.getOrElse(().pure[F])

    val (newHotPeers, updatedPeers) = state.peers.moveToState(toHot, PeerState.Hot)

    Logger[F].infoIf(newHotPeers.nonEmpty, show"Going to hot next hosts: $toHot") >>
    peerUpdateMessagesF(newHotPeers.keySet) >>
    sendReputationUpdateF(newHotPeers.keySet) >>
    state.copy(peers = updatedPeers).pure[F]
  }

}
