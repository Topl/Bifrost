package co.topl.networking.fsnetwork

import cats.Applicative
import cats.data.{NonEmptyChain, OptionT}
import cats.effect.{Async, Resource}
import cats.implicits._
import co.topl.actor.{Actor, Fsm}
import co.topl.algebras.Store
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.validation.algebras.TransactionSyntaxVerifier
import co.topl.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import co.topl.consensus.algebras.{BlockHeaderToBodyValidationAlgebra, LocalChainAlgebra}
import co.topl.consensus.models.{BlockHeader, BlockId, SlotData}
import co.topl.eventtree.{EventSourcedState, ParentChildTree}
import co.topl.ledger.algebras.MempoolAlgebra
import co.topl.networking.blockchain.BlockchainPeerClient
import co.topl.networking.fsnetwork.BlockChecker.BlockCheckerActor
import co.topl.networking.fsnetwork.PeerActor.PeerActor
import co.topl.networking.fsnetwork.PeersManager.Message._
import co.topl.networking.fsnetwork.ReputationAggregator.ReputationAggregatorActor
import co.topl.networking.fsnetwork.RequestsProxy.RequestsProxyActor
import co.topl.networking.p2p.RemoteAddress
import com.github.benmanes.caffeine.cache.Cache
import org.typelevel.log4cats.Logger
import co.topl.networking.fsnetwork.DnsResolverHTInstances._

/**
 * Actor for managing peers
 */
object PeersManager {
  sealed trait Message

  object Message {

    /**
     * Notify Peer manager about local used address
     * @param localAddress used local address
     */
    case class UpdateThisPeerAddress(localAddress: RemoteAddress) extends Message

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
     * Peer connection for closing, connection will be closed even if remote peer is still use it
     *
     * @param hostId closed peer
     */
    case class ClosePeer(hostId: HostId) extends Message

    /**
     * Move peer to cold status, close connection ONLY if remote peer is not used it as well
     * @param hostIds peer for moving to cold state
     */
    case class MoveToCold(hostIds: NonEmptyChain[HostId]) extends Message

    /**
     * Add known peers to peers list
     *
     * @param knownPeers peers to add
     */
    case class AddKnownPeers(knownPeers: NonEmptyChain[RemotePeer]) extends Message

    /**
     * Add known neighbour, i.e. hot peers of remote current hot peer
     * @param knownNeighbors known neighbors
     */
    case class AddKnownNeighbors(source: HostId, knownNeighbors: NonEmptyChain[RemoteAddress]) extends Message

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

    /**
     * Update remote peer application level status
     */
    case class RemotePeerNetworkLevel(hostId: HostId, networkLevel: Boolean) extends Message

    /**
     * Print common ancestor for all physically available connections
     */
    case object PrintCommonAncestor extends Message
  }

  case class State[F[_]: Applicative](
    thisHostIds:                 Set[HostId],
    networkAlgebra:              NetworkAlgebra[F],
    reputationAggregator:        Option[ReputationAggregatorActor[F]],
    blocksChecker:               Option[BlockCheckerActor[F]], // TODO remove it
    requestsProxy:               Option[RequestsProxyActor[F]],
    peersHandler:                PeersHandler[F],
    localChain:                  LocalChainAlgebra[F],
    slotDataStore:               Store[F, BlockId, SlotData],
    transactionStore:            Store[F, TransactionId, IoTransaction],
    blockIdTree:                 ParentChildTree[F, BlockId],
    blockHeights:                EventSourcedState[F, Long => F[Option[BlockId]], BlockId],
    mempool:                     MempoolAlgebra[F],
    headerToBodyValidation:      BlockHeaderToBodyValidationAlgebra[F],
    transactionSyntaxValidation: TransactionSyntaxVerifier[F],
    newPeerCreationAlgebra:      PeerCreationRequestAlgebra[F],
    p2pNetworkConfig:            P2PNetworkConfig,
    coldToWarmSelector:          SelectorColdToWarm[F],
    warmToHotSelector:           SelectorWarmToHot[F],
    hotPeersUpdate:              Set[RemoteAddress] => F[Unit],
    blockSource:                 Cache[BlockId, Set[HostId]]
  ) {

    def sendReputationMessage(message: ReputationAggregator.Message): F[Unit] =
      reputationAggregator.map(_.sendNoWait(message)).getOrElse(Applicative[F].unit)
  }

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
        case (state, PrintCommonAncestor)                          => requestCommonAncestor(state)
        case (state, GetCurrentTips)                               => getCurrentTips(state)
        case (state, RemotePeerServerPort(peer, peerServerPort))   => remotePeerServerPort(state, peer, peerServerPort)
        case (state, RemotePeerNetworkLevel(peer, level))          => remoteNetworkLevel(thisActor, state, peer, level)
        case (state, MoveToCold(peers))                            => coldPeer(thisActor, state, peers)
        case (state, ClosePeer(peer))                              => closePeer(thisActor, state, peer)
        case (state, BanPeer(hostId))                              => banPeer(thisActor, state, hostId)
        case (state, UpdateThisPeerAddress(localAddress))          => addLocalAddress(state, localAddress)
        case (state, newPeer: OpenedPeerConnection[F] @unchecked)  => openedPeerConnection(thisActor, state, newPeer)
        case (state, AddKnownNeighbors(source, peers))             => addKnownNeighbors(state, source, peers)
        case (state, AddKnownPeers(peers))                         => addKnownPeers(state, peers)
        case (state, UpdateWarmHosts(perfReputation))              => updateWarmHosts(thisActor, state, perfReputation)
        case (state, UpdatedReputation(perf, block, novelty))      => repUpdate(thisActor, state, perf, block, novelty)
      }

  def makeActor[F[_]: Async: Logger: DnsResolver](
    thisHostId:                  HostId,
    networkAlgebra:              NetworkAlgebra[F],
    localChain:                  LocalChainAlgebra[F],
    slotDataStore:               Store[F, BlockId, SlotData],
    transactionStore:            Store[F, TransactionId, IoTransaction],
    blockIdTree:                 ParentChildTree[F, BlockId],
    blockHeights:                EventSourcedState[F, Long => F[Option[BlockId]], BlockId],
    mempool:                     MempoolAlgebra[F],
    headerToBodyValidation:      BlockHeaderToBodyValidationAlgebra[F],
    transactionSyntaxValidation: TransactionSyntaxVerifier[F],
    newPeerCreationAlgebra:      PeerCreationRequestAlgebra[F],
    p2pConfig:                   P2PNetworkConfig,
    hotPeersUpdate:              Set[RemoteAddress] => F[Unit],
    savePeersFunction:           Set[RemotePeer] => F[Unit],
    coldToWarmSelector:          SelectorColdToWarm[F],
    warmToHotSelector:           SelectorWarmToHot[F],
    initialPeers:                Map[HostId, Peer[F]],
    blockSource:                 Cache[BlockId, Set[HostId]]
  ): Resource[F, PeersManagerActor[F]] = {
    val initialState =
      PeersManager.State[F](
        Set.empty,
        networkAlgebra,
        reputationAggregator = None,
        blocksChecker = None,
        requestsProxy = None,
        PeersHandler(initialPeers, p2pConfig.networkProperties.closeTimeoutWindowInMs, None),
        localChain,
        slotDataStore,
        transactionStore,
        blockIdTree,
        blockHeights,
        mempool,
        headerToBodyValidation,
        transactionSyntaxValidation,
        newPeerCreationAlgebra,
        p2pConfig,
        coldToWarmSelector,
        warmToHotSelector,
        hotPeersUpdate,
        blockSource
      )

    val actorName = s"Peers manager actor"

    for {
      _     <- Resource.liftK(Logger[F].info(s"Start PeerManager for host $thisHostId"))
      actor <- Actor.makeFull(actorName, initialState, getFsm[F], finalizeActor[F](savePeersFunction))
    } yield actor
  }

  private def finalizeActor[F[_]](
    savePeersFunction: Set[RemotePeer] => F[Unit]
  )(state: State[F]): F[Unit] = savePeersFunction(state.peersHandler.getRemotePeers)

  private def peerReleaseAction[F[_]: Async](thisActor: PeersManagerActor[F])(peer: Peer[F]): F[Unit] =
    peer.actorOpt.map(a => thisActor.releaseActor(a).void).getOrElse(Applicative[F].unit)

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
    val newPeerHandler = state.peersHandler.copy(reputationAggregator = Option(aggregator))
    val newState = state.copy(reputationAggregator = Option(aggregator), peersHandler = newPeerHandler)
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
      source  <- state.peersHandler.getHotPeers.keySet.find(sources.contains)
      peer    <- state.peersHandler.get(source)
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
      d => state.sendReputationMessage(ReputationAggregator.Message.BlockProvidingReputationUpdate(d))

    NonEmptyChain.fromSeq(toSend).map(sendMessage).getOrElse(().pure[F]) >> (state, state).pure[F]
  }

  private def doNetworkQualityMeasure[F[_]: Async](state: State[F]): F[(State[F], Response[F])] =
    state.peersHandler.getWarmPeers.values.toSeq.traverse(_.sendNoWait(PeerActor.Message.GetNetworkQuality)) >>
    (state, state).pure[F]

  private def requestCommonAncestor[F[_]: Async](state: State[F]): F[(State[F], Response[F])] =
    state.peersHandler.forPeersWithActor(_.sendNoWait(PeerActor.Message.PrintCommonAncestor)).sequence >>
    (state, state).pure[F]

  private def getCurrentTips[F[_]: Async: Logger](state: State[F]): F[(State[F], Response[F])] =
    Logger[F].info(s"Got request to get all available tips") >>
    state.peersHandler.getHotPeers.values.toSeq.traverse(_.sendNoWait(PeerActor.Message.GetCurrentTip)) >>
    (state, state).pure[F]

  private def remotePeerServerPort[F[_]: Async: Logger](
    state:      State[F],
    hostId:     HostId,
    serverPort: Int
  ): F[(State[F], Response[F])] =
    for {
      _        <- Logger[F].info(s"For peer $hostId received server address: $serverPort")
      newState <- state.copy(peersHandler = state.peersHandler.copyWithUpdatedServerPort(hostId, serverPort)).pure[F]
      _        <- updateExternalHotPeersList(newState)
    } yield (newState, newState)

  private def remoteNetworkLevel[F[_]: Async: Logger](
    thisActor:          PeersManagerActor[F],
    state:              State[F],
    hostId:             HostId,
    networkLevelStatus: Boolean
  ): F[(State[F], Response[F])] =
    for {
      _ <- Logger[F].info(s"Update remote network level for peer $hostId to $networkLevelStatus")
      newPeersHandler <-
        state.peersHandler.copyWithUpdatedNetworkLevel(Set(hostId), networkLevelStatus, peerReleaseAction(thisActor))
      newState <- state.copy(peersHandler = newPeersHandler).pure[F]
    } yield (newState, newState)

  private def updateExternalHotPeersList[F[_]: Async: Logger](state: State[F]): F[Unit] =
    for {
      hotPeersServers <- state.peersHandler.getHotPeers.values.flatMap(_.asRemoteAddress).toSet.pure[F]
      _               <- Logger[F].debug(s"Going to update hot peers servers $hotPeersServers")
      _               <- state.hotPeersUpdate(hotPeersServers)
    } yield ()

  private def coldPeer[F[_]: Async: Logger](
    thisActor: PeersManagerActor[F],
    state:     State[F],
    hostIds:   NonEmptyChain[HostId]
  ): F[(State[F], Response[F])] =
    stopPeerActivity(thisActor, state, hostIds.toList.toSet, PeerState.Cold).map(newState => (newState, newState))

  // Disable application level AND close connection as well for peer
  private def closePeer[F[_]: Async: Logger](
    thisActor: PeersManagerActor[F],
    state:     State[F],
    hostId:    HostId
  ): F[(State[F], Response[F])] =
    closePeers(thisActor, state, Set(hostId), PeerState.Cold).map(newState => (newState, newState))

  // Disable application level AND close connection as well for peers
  private def closePeers[F[_]: Async: Logger](
    thisActor: PeersManagerActor[F],
    state:     State[F],
    hostIds:   Set[HostId],
    endState:  PeerState
  ): F[State[F]] = {
    require(!endState.isActive)
    for {
      _        <- Logger[F].info(s"Going to clean-up peer actor for $hostIds due to closed connection")
      newState <- stopPeerActivity(thisActor, state, hostIds, endState)
      peers <- newState.peersHandler.copyWithUpdatedNetworkLevel(
        hostIds,
        netLevel = false,
        peerReleaseAction(thisActor)
      )
    } yield newState.copy(peersHandler = peers)
  }

  private def stopPeerActivity[F[_]: Async: Logger](
    thisActor: PeersManagerActor[F],
    state:     State[F],
    hostIds:   Set[HostId],
    endStatus: PeerState
  ): F[State[F]] =
    if (hostIds.nonEmpty) {
      require(!endStatus.isActive)

      for {
        _              <- Logger[F].info(s"Going to stop network and application level for peers $hostIds")
        newPeerHandler <- state.peersHandler.moveToState(hostIds, endStatus, peerReleaseAction(thisActor))
        newState       <- state.copy(peersHandler = newPeerHandler).pure[F]
      } yield newState
    } else {
      state.pure[F]
    }

  private def banPeer[F[_]: Async: Logger](
    thisActor: PeersManagerActor[F],
    state:     State[F],
    hostId:    HostId
  ): F[(State[F], Response[F])] =
    for {
      _        <- Logger[F].info(s"Going to ban peer $hostId")
      newState <- stopPeerActivity(thisActor, state, Set(hostId), PeerState.Banned)
    } yield (newState, newState)

  private def addLocalAddress[F[_]: Async: Logger](
    state:            State[F],
    localPeerAddress: RemoteAddress
  ): F[(State[F], Response[F])] = {
    val newState = state.copy(thisHostIds = state.thisHostIds + localPeerAddress.host)
    Logger[F].info(s"Added ${localPeerAddress.host} as known local address") >>
    (newState, newState).pure[F]
  }

  private def openedPeerConnection[F[_]: Async: Logger](
    thisActor: PeersManagerActor[F],
    state:     State[F],
    setupPeer: OpenedPeerConnection[F]
  ): F[(State[F], Response[F])] = {
    val hostId: HostId = setupPeer.address.host
    val client: BlockchainPeerClient[F] = setupPeer.client

    require(state.requestsProxy.isDefined)
    require(state.reputationAggregator.isDefined)

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
            state.blockHeights,
            state.headerToBodyValidation,
            state.transactionSyntaxValidation,
            state.mempool
          )
        )

    if (state.peersHandler.hostIsNotBanned(hostId)) {
      if (state.peersHandler.haveNoActorForHost(hostId)) {
        for {
          _              <- Logger[F].info(show"Going to create actor for handling connection to remote peer $hostId")
          peerActor      <- setupPeerActor
          newPeerHandler <- state.peersHandler.copyWithNewActor(hostId).copyWithNewPeerActor(hostId, peerActor)
          newState = state.copy(peersHandler = newPeerHandler)
        } yield (newState, newState)
      } else {
        Logger[F].error(show"Try to redefine actor for remote peer $hostId") >>
        (state, state).pure[F]
      }
    } else {
      Logger[F].warn(show"Actor for $hostId was not created because peer is banned. Connection will be closed") >>
      client.closeConnection() >>
      (state, state).pure[F]
    }
  }

  private def resolveHosts[T, F[_]: Async: Logger](unresolved: Seq[T])(implicit res: DnsResolverHT[T, F]): F[Seq[T]] =
    unresolved
      .traverse { unresolvedAddress =>
        unresolvedAddress
          .resolve()
          .flatTap(resolvedAddress => Logger[F].debug(s"Resolve address $unresolvedAddress to $resolvedAddress"))
      }
      .map(_.flatten.toSeq)

  private def addKnownNeighbors[F[_]: Async: Logger: DnsResolver](
    state:      State[F],
    source:     HostId,
    knownPeers: NonEmptyChain[RemoteAddress]
  ): F[(State[F], Response[F])] = {
    for {
      resolvedPeers    <- OptionT(resolveHosts(knownPeers.toList).map(NonEmptyChain.fromSeq))
      filteredLoopback <- OptionT.fromOption[F](NonEmptyChain.fromChain(resolvedPeers.filterNot(_.isSpecialHost)))
    } yield {
      val sourcePeer = state.peersHandler.get(source)
      val initialBlockRep = sourcePeer.map(_.lastKnownBlockProvidingReputation).getOrElse(0.0)
      val initialPerfRep = sourcePeer.map(_.lastKnownPerformanceReputation).getOrElse(0.0)

      val peerToAdd =
        filteredLoopback.map(ra => RemotePeer(ra, initialBlockRep, initialPerfRep))
      addKnownResolvedPeers(state, peerToAdd)
    }
  }.getOrElse((state, state).pure[F]).flatten

  private def addKnownPeers[F[_]: Async: Logger: DnsResolver](
    state:      State[F],
    knownPeers: NonEmptyChain[RemotePeer]
  ): F[(State[F], Response[F])] = {
    for {
      resolvedPeers <- OptionT(resolveHosts(knownPeers.toList).map(NonEmptyChain.fromSeq))
    } yield addKnownResolvedPeers(state, resolvedPeers)
  }.getOrElse((state, state).pure[F]).flatten

  private def addKnownResolvedPeers[F[_]: Async: Logger](
    state:      State[F],
    knownPeers: NonEmptyChain[RemotePeer]
  ): F[(State[F], Response[F])] =
    for {
      oldPeers            <- state.peersHandler.pure[F]
      newPeers            <- oldPeers.copyWithAddedPeers(knownPeers).pure[F]
      peersHadBeenChanged <- (newPeers.peers.size != oldPeers.peers.size).pure[F]
      _                   <- Logger[F].infoIf(peersHadBeenChanged, s"Add some peers from: $knownPeers")
      newState            <- state.copy(peersHandler = newPeers).pure[F]
    } yield (newState, newState)

  private def updateWarmHosts[F[_]: Async: Logger](
    thisActor:             PeersManagerActor[F],
    state:                 State[F],
    performanceReputation: Map[HostId, HostReputationValue]
  ): F[(State[F], Response[F])] = {
    val minimumWarmPeers = state.p2pNetworkConfig.networkProperties.minimumWarmConnections
    val warmHostsByReputation =
      state.peersHandler.getWarmPeers
        .map { case (host, _) => (host, performanceReputation.getOrElse(host, 0.0)) }
        .toList
        .sortBy(_._2)
    val warmHostsSize = warmHostsByReputation.size
    val warmToCold = warmHostsByReputation.take((warmHostsSize - minimumWarmPeers) / 2).map(_._1)

    for {
      _        <- Logger[F].infoIf(warmToCold.nonEmpty, s"Update warm hosts, set warm hosts $warmToCold to cold state")
      newState <- stopPeerActivity(thisActor, state, warmToCold.toSet, PeerState.Cold)
      _        <- requestNeighboursFromHotPeers(state)
    } yield (newState, newState)
  }

  private def requestNeighboursFromHotPeers[F[_]: Async: Logger](state: State[F]): F[Unit] = {
    val newWarmPeerCount = state.p2pNetworkConfig.networkProperties.minimumWarmConnections
    val maximumWarmConnections = state.p2pNetworkConfig.networkProperties.maximumWarmConnections
    val warmPeersSize = state.peersHandler.getWarmPeers.size

    if (warmPeersSize < maximumWarmConnections && newWarmPeerCount > 0) {
      val currentHotPeers = state.peersHandler.getHotPeers
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
      _ <- Logger[F].debug(s"Peers: ${state.peersHandler}")
      _ <- Logger[F].debug(show"Got update: Performance: $perfRep; Block: $blockRep; Novelty: $noveltyRep")

      stateWithLastRep     <- updateLastKnowReputationForHotPeers(state, perfRepDefault, blockRepDefault).pure[F]
      stateWithPreWarm     <- coldToWarm(thisActor, stateWithLastRep)
      stateWithClosedPeers <- hotToCold(thisActor, stateWithPreWarm, perfRepDefault, blockRepDefault, noveltyRepDefault)
      stateWithNewHotPeers <- warmToHot(thisActor, stateWithClosedPeers)
      _                    <- updateExternalHotPeersList(stateWithNewHotPeers)
    } yield (stateWithNewHotPeers, stateWithNewHotPeers)
  }

  private def updateLastKnowReputationForHotPeers[F[_]: Async](
    state:    State[F],
    perfRep:  Map[HostId, HostReputationValue],
    blockRep: Map[HostId, HostReputationValue]
  ): State[F] = {
    val newPeersHandler =
      state.peersHandler.copyWithUpdatedBlockProviding(blockRep).copyWithUpdatedPerformanceProviding(perfRep)

    state.copy(peersHandler = newPeersHandler)
  }

  private def hotToCold[F[_]: Async: Logger](
    thisActor:  PeersManagerActor[F],
    state:      State[F],
    perfRep:    Map[HostId, HostReputationValue],
    blockRep:   Map[HostId, HostReputationValue],
    noveltyRep: Map[HostId, Long]
  ): F[State[F]] = {
    val currentHotPeers = state.peersHandler.getHotPeers.keySet
    val hotToCold = getHostsToCold(currentHotPeers, state.p2pNetworkConfig, perfRep, blockRep, noveltyRep)

    for {
      _ <- Logger[F].infoIf(
        hotToCold.nonEmpty,
        s"Going to cold $hotToCold due of bad reputation. Reputations:$perfRep; $blockRep; $noveltyRep"
      )
      newState <- stopPeerActivity(thisActor, state, hotToCold, PeerState.Cold)
    } yield newState
  }

  private def warmToHot[F[_]: Async: Logger](
    thisActor: PeersManagerActor[F],
    state:     State[F]
  ): F[State[F]] = {
    val minimumHotConnections = state.p2pNetworkConfig.networkProperties.minimumHotConnections
    val lackHotPeersCount = minimumHotConnections - state.peersHandler.getHotPeers.size
    val warmToHot = getNewHotConnectionHosts(state, lackHotPeersCount)

    warmPeersToHot(thisActor, state, warmToHot)
  }

  private def getNewHotConnectionHosts[F[_]](
    state:       State[F],
    countToOpen: Int
  ): Set[RemoteAddress] =
    state.warmToHotSelector.select(state.peersHandler.getWarmPeers.values.toSet, countToOpen)

  private def coldToWarm[F[_]: Async: Logger: DnsResolver](
    thisActor: PeersManagerActor[F],
    state:     State[F]
  ): F[State[F]] = {
    val minimumWarmConnection = state.p2pNetworkConfig.networkProperties.minimumWarmConnections
    val lackWarmPeersCount = minimumWarmConnection - state.peersHandler.getWarmPeers.size
    val coldPeers: Set[Peer[F]] = state.peersHandler.getColdPeers.values.toSet
    val coldToWarm: Set[HostId] = state.coldToWarmSelector.select(coldPeers, lackWarmPeersCount).map(_.host)

    for {
      peersHandler <- state.peersHandler.moveToState(coldToWarm, PeerState.Warm, peerReleaseAction(thisActor))
      newState     <- state.copy(peersHandler = peersHandler).pure[F]
      warmHostToPeer <- coldToWarm
        .flatMap(host => newState.peersHandler.get(host).map(peer => host -> peer))
        .toMap
        .pure[F]
      _ <- checkConnection(newState, warmHostToPeer)
    } yield newState
  }

  private def checkConnection[F[_]: Async: Logger: DnsResolver](
    state:        State[F],
    peersToCheck: Map[HostId, Peer[F]]
  ): F[Unit] = {
    val addressesToOpen =
      peersToCheck
        .filter { case (_, peer) => peer.haveNoConnection }
        .map(d => RemoteAddress(d._1, d._2.remoteServerPort.get, d._2.resolveDns))
        .toSeq

    for {
      resolved      <- resolveHosts(addressesToOpen)
      nonLocalHosts <- resolved.filterNot(ra => state.thisHostIds.contains(ra.host)).pure[F]
      filtered      <- nonLocalHosts.filter(ra => state.peersHandler.haveNoActorForHost(ra.host)).pure[F]
      _             <- Logger[F].infoIf(filtered.nonEmpty, s"Going to open connection to next peers: $filtered")
      _             <- filtered.traverse(state.newPeerCreationAlgebra.requestNewPeerCreation)
    } yield ()
  }

  private def getHostsToCold(
    currentHotPeers:          Set[HostId],
    p2pNetworkConfig:         P2PNetworkConfig,
    performanceReputation:    Map[HostId, HostReputationValue],
    blockProvidingReputation: Map[HostId, HostReputationValue],
    noveltyReputation:        Map[HostId, Long]
  ): Set[HostId] = {

    // TODO adjust performance reputation to avoid remote peer with best performance reputation but
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
        val totalRep = getTotalReputation(blockProvidingRep, performanceRep)

        totalRep >= p2pNetworkConfig.networkProperties.minimumRequiredReputation
      }

    val allKeptConnections =
      saveByNovelty ++ saveByBlockProviding ++ saveByPerformanceReputation ++ saveByOverallReputation

    currentHotPeers -- allKeptConnections
  }

  private def warmPeersToHot[F[_]: Async: Logger](
    thisActor: PeersManagerActor[F],
    state:     State[F],
    toHot:     Set[RemoteAddress]
  ): F[State[F]] =
    for {
      _            <- Logger[F].infoIf(toHot.nonEmpty, show"Going to hot next hosts: $toHot")
      updatedPeers <- state.peersHandler.moveToState(toHot.map(_.host), PeerState.Hot, peerReleaseAction(thisActor))
    } yield state.copy(peersHandler = updatedPeers)

}
