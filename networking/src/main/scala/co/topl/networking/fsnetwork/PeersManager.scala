package co.topl.networking.fsnetwork

import cats.{Parallel, Show}
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
import co.topl.networking.fsnetwork.RequestsProxy.RequestsProxyActor
import co.topl.networking.p2p.RemoteAddress
import com.github.benmanes.caffeine.cache.Cache
import org.typelevel.log4cats.Logger
import co.topl.networking.fsnetwork.DnsResolverHTInstances._
import co.topl.networking.fsnetwork.ReverseDnsResolverHTInstances._

/**
 * Actor for managing peers
 */
// scalastyle:off number.of.methods
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
     * PingPong message from remote peer, which allow us to measure performance reputation for remote host
     * without exchange any application level information
     *
     * @param hostId   remote peer
     * @param response ping pong message response
     */
    case class PingPongMessagePing(hostId: HostId, response: Either[NetworkQualityError, Long]) extends Message

    /**
     * Information about how long it takes to download block header from remote host,
     * allow us to update performance reputation
     *
     * @param hostId remote peer
     * @param delay  header download time
     */
    case class DownloadTimeHeader(hostId: HostId, delay: Long) extends Message

    /**
     * Information about how long it takes to download block body from remote host,
     * allow us to update performance reputation
     *
     * @param hostId    remote peer
     * @param bodyDelay body download time
     * @param txDelays  transactions download time
     */
    case class DownloadTimeBody(hostId: HostId, bodyDelay: Long, txDelays: Seq[Long]) extends Message

    /**
     * Remote peer provide us remote slot data with better height than our current local chain,
     * but whole slot data chain turned out to be worse of local chain because of density rule
     *
     * @param hostId remote peer
     */
    case class BadKLookbackSlotData(hostId: HostId) extends Message

    /**
     * Remote peer provide to us incorrect block or any other data like genesis block.
     * For example it could be block with incorrect transaction(s)
     *
     * @param hostId remote peer
     */
    case class CriticalErrorForHost(hostId: HostId) extends Message

    /**
     * We got unknown error during get data from remote peer. That error could be network error, for example
     *
     * @param hostId remote peer
     */
    case class NonCriticalErrorForHost(hostId: HostId) extends Message

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
     */
    case object UpdatedReputationTick extends Message

    /**
     * Request measuring network quality from remote warm hosts
     */
    case object GetNetworkQualityForWarmHosts extends Message

    /**
     * Try to connect to new peers
     */
    case object AggressiveP2PUpdate extends Message

    /**
     * Update warm hosts list
     */
    case object UpdateWarmHosts extends Message

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

  case class State[F[_]](
    thisHostIds:                 Set[HostId],
    networkAlgebra:              NetworkAlgebra[F],
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
  )

  type Response[F[_]] = State[F]
  type PeersManagerActor[F[_]] = Actor[F, Message, Response[F]]

  // scalastyle:off cyclomatic.complexity
  def getFsm[F[_]: Async: Parallel: Logger: DnsResolver: ReverseDnsResolver]
    : PeersManagerActor[F] => Fsm[F, State[F], Message, Response[F]] =
    (thisActor: PeersManagerActor[F]) =>
      Fsm {
        case (state, checker: SetupBlockChecker[F] @unchecked)    => setupBlockChecker(state, checker.blockChecker)
        case (state, checker: SetupRequestsProxy[F] @unchecked)   => setupRequestsProxy(state, checker.requestsProxy)
        case (state, PingPongMessagePing(hostId, pongResponse))   => pongMessage(thisActor, state, hostId, pongResponse)
        case (state, DownloadTimeHeader(hostId, delay))           => headerDownloadTime(state, hostId, delay)
        case (state, DownloadTimeBody(hostId, delay, txDelays))   => blockDownloadTime(state, hostId, delay, txDelays)
        case (state, BadKLookbackSlotData(hostId))                => badKLookbackSlotData(thisActor, state, hostId)
        case (state, CriticalErrorForHost(hostId))                => criticalErrorForHost(thisActor, state, hostId)
        case (state, NonCriticalErrorForHost(hostId))             => nonCriticalErrorForHost(thisActor, state, hostId)
        case (state, BlockHeadersRequest(hostId, blocks))         => blockHeadersRequest(state, hostId, blocks)
        case (state, BlockBodyRequest(hostId, blockHeaders))      => blockDownloadRequest(state, hostId, blockHeaders)
        case (state, BlocksSource(sources))                       => blocksSourceProcessing(state, sources)
        case (state, GetNetworkQualityForWarmHosts)               => doNetworkQualityMeasure(state)
        case (state, PrintCommonAncestor)                         => requestCommonAncestor(state)
        case (state, GetCurrentTips)                              => getCurrentTips(state)
        case (state, RemotePeerServerPort(peer, peerServerPort))  => remotePeerServerPort(state, peer, peerServerPort)
        case (state, RemotePeerNetworkLevel(peer, level))         => remoteNetworkLevel(thisActor, state, peer, level)
        case (state, MoveToCold(peers))                           => coldPeer(thisActor, state, peers)
        case (state, ClosePeer(peer))                             => closePeer(thisActor, state, peer)
        case (state, BanPeer(hostId))                             => banPeer(thisActor, state, hostId)
        case (state, UpdateThisPeerAddress(localAddress))         => addLocalAddress(state, localAddress)
        case (state, newPeer: OpenedPeerConnection[F] @unchecked) => openedPeerConnection(thisActor, state, newPeer)
        case (state, AddKnownNeighbors(source, peers))            => addKnownNeighbors(state, source, peers)
        case (state, AddKnownPeers(peers))                        => addKnownPeers(state, peers)
        case (state, UpdateWarmHosts)                             => updateWarmHosts(state)
        case (state, AggressiveP2PUpdate)                         => aggressiveP2PUpdate(thisActor, state)
        case (state, UpdatedReputationTick)                       => repUpdate(thisActor, state)
      }
  // scalastyle:on cyclomatic.complexity

  // scalastyle:off parameter.number
  def makeActor[F[_]: Async: Parallel: Logger: DnsResolver: ReverseDnsResolver](
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
        blocksChecker = None,
        requestsProxy = None,
        PeersHandler(initialPeers, p2pConfig),
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
      _          <- Resource.liftK(Logger[F].info(s"Start PeerManager for host $thisHostId"))
      thisHostIp <- Resource.liftK(thisHostId.resolving())
      state <- Resource.pure(thisHostIp.map(ip => initialState.copy(thisHostIds = Set(ip))).getOrElse(initialState))
      actor <- Actor.makeFull(actorName, state, getFsm[F], finalizeActor[F](savePeersFunction))
    } yield actor
  }
  // scalastyle:on parameter.number

  private def finalizeActor[F[_]: Async: Logger](
    savePeersFunction: Set[RemotePeer] => F[Unit]
  )(state: State[F])(implicit res: ReverseDnsResolverHT[RemotePeer, F]): F[Unit] =
    for {
      ipToSave  <- state.peersHandler.getRemotePeers.toSeq.pure[F]
      _         <- Logger[F].debug(show"Try to resolve next ip(s) as hostnames: $ipToSave")
      hostNames <- state.peersHandler.getRemotePeers.toSeq.traverse(_.reverseResolving())
      _         <- Logger[F].debug(show"Resolved ip(s) as hostnames: $hostNames")
      _         <- savePeersFunction(hostNames.toSet)
    } yield ()

  private def peerReleaseAction[F[_]: Async](thisActor: PeersManagerActor[F])(peer: Peer[F]): F[Unit] =
    peer.actorOpt.traverse_(a => thisActor.releaseActor(a).void)

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

  def delayToReputation(networkConfig: P2PNetworkConfig, delayInMs: Long): HostReputationValue = {
    val reputationReducing = delayInMs.toDouble / networkConfig.performanceReputationMaxDelay

    networkConfig.performanceReputationInitialValue - reputationReducing
  }

  private def pongMessage[F[_]: Async: Logger](
    thisActor: PeersManagerActor[F],
    state:     State[F],
    hostId:    HostId,
    response:  Either[NetworkQualityError, Long]
  ): F[(State[F], Response[F])] =
    response match {
      case Right(value) =>
        val newReputation = delayToReputation(state.p2pNetworkConfig, value)
        val newPeersHandler = state.peersHandler.copyWithUpdatedReputation(perfRepMap = Map(hostId -> newReputation))
        val newState = state.copy(peersHandler = newPeersHandler)
        Logger[F].info(show"Got pong message delay $value from remote host $hostId") >>
        (newState, newState).pure[F]

      case Left(error) =>
        Logger[F].error(show"Bad pong message: $error from host $hostId") >>
        banPeer(thisActor, state, hostId)
    }

  private def updatePerfRepWithDelay[F[_]: Async](
    state:  State[F],
    hostId: HostId,
    delay:  Long
  ): F[(State[F], Response[F])] = {
    val newReputation = delayToReputation(state.p2pNetworkConfig, delay)
    val oldReputation = state.peersHandler.get(hostId).map(_.perfRep)
    val updatedReputation =
      oldReputation.map(oldValue => (oldValue * 2 + newReputation) / 3.0).getOrElse(newReputation)

    val newPeersHandler =
      state.peersHandler.copyWithUpdatedReputation(perfRepMap = Map(hostId -> updatedReputation))
    val newState = state.copy(peersHandler = newPeersHandler)

    (newState, newState).pure[F]
  }

  private def headerDownloadTime[F[_]: Async: Logger](
    state:  State[F],
    hostId: HostId,
    delay:  Long
  ): F[(State[F], Response[F])] =
    Logger[F].info(show"Received header download from host $hostId with delay $delay") >>
    updatePerfRepWithDelay(state, hostId, delay)

  private def blockDownloadTime[F[_]: Async: Logger](
    state:    State[F],
    hostId:   HostId,
    delay:    Long,
    tsDelays: Seq[Long]
  ): F[(State[F], Response[F])] = {
    val maxDelay = (tsDelays :+ delay).max
    Logger[F].debug(show"Received block download from host $hostId with max delay $delay") >>
    updatePerfRepWithDelay(state, hostId, maxDelay)
  }

  private def badKLookbackSlotData[F[_]: Async: Logger](
    thisActor: PeersManagerActor[F],
    state:     State[F],
    hostId:    HostId
  ): F[(State[F], Response[F])] =
    Logger[F].error(show"Got bad k lookback slot data from host $hostId") >>
    coldPeer(thisActor, state, NonEmptyChain.one(hostId))

  private def criticalErrorForHost[F[_]: Async: Logger](
    thisActor: PeersManagerActor[F],
    state:     State[F],
    hostId:    HostId
  ): F[(State[F], Response[F])] =
    Logger[F].error(show"Received critical error from host $hostId") >>
    banPeer(thisActor, state, hostId)

  private def nonCriticalErrorForHost[F[_]: Async: Logger](
    thisActor: PeersManagerActor[F],
    state:     State[F],
    hostId:    HostId
  ): F[(State[F], Response[F])] =
    Logger[F].error(show"Got non critical error during receiving data from host $hostId") >>
    coldPeer(thisActor, state, NonEmptyChain.one(hostId))

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
          .traverse_(_.sendNoWait(BlockChecker.Message.InvalidateBlockIds(NonEmptyChain.one(blockIds.last))))

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
          .traverse_(_.sendNoWait(BlockChecker.Message.InvalidateBlockIds(NonEmptyChain.one(blocks.last.id))))
        (state, state).pure[F]
    }

  private def getHotPeerByBlockId[F[_]](state: State[F], blockId: BlockId, hostId: Option[HostId]): Option[Peer[F]] =
    for {
      sources <- Option(state.blockSource.getOrElse(blockId, Set.empty) ++ hostId.toSet)
      source  <- state.peersHandler.getHotPeers.keySet.find(sources.contains)
      peer    <- state.peersHandler.get(source)
    } yield peer

  def knownSourcesToReputation(networkConfig: P2PNetworkConfig, knownSources: Long): HostReputationValue = {
    val reputationReducing: HostReputationValue = (knownSources - 1) * networkConfig.blockNoveltyReputationStep
    Math.max(networkConfig.blockNoveltyInitialValue - reputationReducing, 0)
  }

  private def blocksSourceProcessing[F[_]: Async](
    state:   State[F],
    sources: NonEmptyChain[(HostId, BlockId)]
  ): F[(State[F], Response[F])] = {
    val peerToKnownSource: Map[HostId, Int] =
      sources.toList
        .flatMap { case (host, blockId) =>
          val previousSource: Set[HostId] = state.blockSource.get(blockId).getOrElse(Set.empty[HostId])
          if (previousSource.contains(host)) {
            Option.empty[(HostId, Int)]
          } else {
            val newSource: Set[HostId] = previousSource + host
            state.blockSource.put(blockId, newSource)
            Option((host, newSource.size))
          }
        }
        .groupMapReduce(_._1)(_._2)(Math.min)

    val perfRepUpdate: Map[HostId, HostReputationValue] =
      peerToKnownSource
        .map { case (source, knownSourceCount) =>
          val newReputation = knownSourcesToReputation(state.p2pNetworkConfig, knownSourceCount)
          val oldReputation = state.peersHandler.get(source).map(_.blockRep).getOrElse(0.0)
          (source, Math.max(newReputation, oldReputation))
        }

    val noveltyRepUpdate =
      peerToKnownSource
        .filter(_._2 == 1) // select peer which provide completely new block
        .map { case (source, _) => source -> state.p2pNetworkConfig.remotePeerNoveltyInSlots }

    val newPeerHandler =
      state.peersHandler.copyWithUpdatedReputation(blockRepMap = perfRepUpdate, noveltyRepMap = noveltyRepUpdate)
    val newState = state.copy(peersHandler = newPeerHandler)

    (newState, newState).pure[F]
  }

  private def doNetworkQualityMeasure[F[_]: Async](state: State[F]): F[(State[F], Response[F])] =
    state.peersHandler.getWarmPeersWithActor.values.toSeq.traverse(_.sendNoWait(PeerActor.Message.GetNetworkQuality)) >>
    (state, state).pure[F]

  private def requestCommonAncestor[F[_]: Async: Logger](state: State[F]): F[(State[F], Response[F])] =
    Logger[F].info(show"Current hot peer(s) state: ${state.peersHandler.getHotPeers}") >>
    Logger[F].info(show"Current warm peer(s) state: ${state.peersHandler.getWarmPeers}") >>
    Logger[F].info(show"Current first five cold peer(s) state: ${state.peersHandler.getColdPeers.take(5)}") >>
    Logger[F].info(show"With known cold peers: ${state.peersHandler.getColdPeers.keySet}") >>
    state.peersHandler.forPeersWithActor(_.sendNoWait(PeerActor.Message.PrintCommonAncestor)).sequence >>
    (state, state).pure[F]

  private def getCurrentTips[F[_]: Async: Logger](state: State[F]): F[(State[F], Response[F])] =
    Logger[F].info(s"Got request to get all available tips") >>
    state.peersHandler.getHotPeers.values.toSeq.traverse(_.sendNoWait(PeerActor.Message.GetCurrentTip)) >>
    (state, state).pure[F]

  private def remotePeerServerPort[F[_]: Async: Parallel: Logger: ReverseDnsResolver](
    state:      State[F],
    hostId:     HostId,
    serverPort: Int
  ): F[(State[F], Response[F])] =
    for {
      _ <- Logger[F].info(s"For peer $hostId received server address: $serverPort")
      newState <- state
        .copy(peersHandler = state.peersHandler.copyWithUpdatedServerPort(hostId, serverPort.some))
        .pure[F]
      _ <- updateExternalHotPeersList(newState)
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

  private def updateExternalHotPeersList[F[_]: Async: Parallel: Logger](
    state: State[F]
  )(implicit res: ReverseDnsResolverHT[RemoteAddress, F]): F[Unit] =
    for {
      hotPeersServers <- state.peersHandler.getHotPeers.values.flatMap(_.asRemoteAddress).toSet.pure[F]
      _               <- Logger[F].debug(show"Going to resolve ip(s) to hostnames for hot peers $hotPeersServers")
      hotPeersAsHosts <- hotPeersServers.toSeq.parTraverse(_.reverseResolving())
      _               <- Logger[F].debug(show"Going to update hot peers hostnames $hotPeersAsHosts")
      _               <- state.hotPeersUpdate(hotPeersAsHosts.toSet)
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

    def setupPeerActor: F[PeerActor[F]] =
      thisActor
        .acquireActor(() =>
          state.networkAlgebra.makePeer(
            hostId,
            state.networkAlgebra,
            client,
            state.requestsProxy.get,
            thisActor,
            state.localChain,
            state.slotDataStore,
            state.transactionStore,
            state.blockIdTree,
            state.blockHeights,
            state.headerToBodyValidation,
            state.transactionSyntaxValidation,
            state.mempool,
            state.p2pNetworkConfig.networkProperties.slotDataDownloadStep,
            commonAncestor
          )
        )

    if (state.peersHandler.hostIsNotBanned(hostId)) {
      if (state.peersHandler.haveNoActorForHost(hostId)) {
        for {
          _         <- Logger[F].info(show"Going to create actor for handling connection to remote peer $hostId")
          peerActor <- setupPeerActor
          // shall do in two step, for case of incoming connection from already known cold peer
          newPeerHandler <-
            state.peersHandler
              .copyWithNewHost(hostId)
              .copyWithNewPeerActor(hostId, peerActor)
              // clear port in case if expose server port is not allowed by remote peer
              .copyWithUpdatedServerPort(hostId, None)
              .pure[F]

          peerState = newPeerHandler.get(hostId).get.state
          _ <- peerActor.sendNoWait(PeerActor.Message.UpdateState(peerState.networkLevel, peerState.applicationLevel))
          _ <- peerActor.sendNoWait(PeerActor.Message.GetNetworkQuality)
          _ <- peerActor.sendNoWait(PeerActor.Message.GetPeerServerAddress)

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

  private def resolveHosts[T: Show, F[_]: Async: Parallel: Logger](
    unresolved: Seq[T]
  )(implicit res: DnsResolverHT[T, F]): F[Seq[T]] =
    unresolved
      .parTraverse { unresolvedAddress =>
        unresolvedAddress
          .resolving()
          .flatTap(resolvedAddress => Logger[F].debug(show"Resolve address $unresolvedAddress to $resolvedAddress"))
      }
      .map(_.flatten.toSeq)

  private def addKnownNeighbors[F[_]: Async: Parallel: Logger: DnsResolver](
    state:      State[F],
    source:     HostId,
    knownPeers: NonEmptyChain[RemoteAddress]
  ): F[(State[F], Response[F])] = {
    for {
      resolvedPeers   <- OptionT(resolveHosts(knownPeers.toList).map(NonEmptyChain.fromSeq))
      nonSpecialHosts <- OptionT.fromOption[F](NonEmptyChain.fromChain(resolvedPeers.filterNot(_.isSpecialHost)))
      neighbourBlockRep = state.peersHandler.get(source).map(_.blockRep).getOrElse(0.0)
      peerToAdd = nonSpecialHosts.map(ra => RemotePeer(ra, neighbourBlockRep, 0.0))
    } yield addKnownResolvedPeers(state, peerToAdd)
  }.getOrElse((state, state).pure[F]).flatten

  private def addKnownPeers[F[_]: Async: Parallel: Logger: DnsResolver](
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
      filteredLoopback    <- knownPeers.filterNot(ra => state.thisHostIds.contains(ra.address.host)).pure[F]
      oldPeers            <- state.peersHandler.pure[F]
      newPeers            <- oldPeers.copyWithAddedPeers(filteredLoopback).pure[F]
      peersHadBeenChanged <- (newPeers.peers.size != oldPeers.peers.size).pure[F]
      _                   <- Logger[F].infoIf(peersHadBeenChanged, show"New known peers: $filteredLoopback")
      newState            <- state.copy(peersHandler = newPeers).pure[F]
    } yield (newState, newState)

  private def updateWarmHosts[F[_]: Async: Logger](state: State[F]): F[(State[F], Response[F])] =
    for {
      _        <- requestNeighboursFromHotPeers(state)
      newState <- clearColdPeers(state).pure[F]
    } yield (newState, newState)

  private def requestNeighboursFromHotPeers[F[_]: Async: Logger](state: State[F]): F[Unit] = {
    val newWarmPeerCount = state.p2pNetworkConfig.networkProperties.maximumWarmConnections
    val warmPeersSize = state.peersHandler.getWarmPeersWithActor.size

    if (warmPeersSize < newWarmPeerCount && newWarmPeerCount > 0) {
      val currentHotPeers = state.peersHandler.getHotPeers
      Logger[F].debug(s"Request neighbour(s) from peers: ${currentHotPeers.keySet}") >>
      currentHotPeers.values.toList
        .traverse(_.sendNoWait(PeerActor.Message.GetHotPeersFromPeer(newWarmPeerCount)))
        .void
    } else {
      Logger[F].info(s"Do not request neighbours, warmPeersSize: $warmPeersSize, newWarmPeerCount: $newWarmPeerCount")
    }
  }

  // we do NOT work here with cold peers which are present but had been closed recently. That allow us to not remove
  // cold peer which are no eligible to move to warm status
  private def clearColdPeers[F[_]](state: State[F]): State[F] = {
    val eligibleColdPeers = getEligibleColdPeers(state).filter(_._2.remoteServerPort.isDefined)
    if (eligibleColdPeers.size > state.p2pNetworkConfig.networkProperties.maximumEligibleColdConnections) {
      val minimumColdConnections = state.p2pNetworkConfig.networkProperties.minimumEligibleColdConnections
      val savedCold =
        eligibleColdPeers.toSeq.sortBy(_._2.closedTimestamps.size).take(minimumColdConnections).map(_._1).toSet
      val newPeer = state.peersHandler.copyWithRemovedColdPeersWithoutActor(eligibleColdPeers.keySet -- savedCold)
      state.copy(peersHandler = newPeer)
    } else {
      state
    }
  }

  private def aggressiveP2PUpdate[F[_]: Async: Logger](
    thisActor: PeersManagerActor[F],
    state:     State[F]
  ): F[(State[F], Response[F])] = {
    val toHot =
      state.warmToHotSelector.select(
        state.peersHandler.getWarmPeersWithActor,
        state.p2pNetworkConfig.networkProperties.aggressiveP2PCount
      )

    warmPeersToHot(thisActor, state, toHot).map(s => (s, s))
  }

  private def repUpdate[F[_]: Async: Parallel: Logger: DnsResolver: ReverseDnsResolver](
    thisActor: PeersManagerActor[F],
    state:     State[F]
  ): F[(State[F], Response[F])] =
    for {
      _ <- Logger[F].debug(s"Peers: ${state.peersHandler}")

      stateWithLastRep     <- getPeerHandlerAfterReputationDecoy(state).pure[F]
      stateWithPreWarm     <- coldToWarm(thisActor, stateWithLastRep)
      stateWithClosedPeers <- hotToCold(thisActor, stateWithPreWarm)
      stateWithNewHotPeers <- warmToHot(thisActor, stateWithClosedPeers)
      _                    <- updateExternalHotPeersList(stateWithNewHotPeers)
    } yield (stateWithNewHotPeers, stateWithNewHotPeers)

  private def getPeerHandlerAfterReputationDecoy[F[_]](
    state: State[F]
  ): State[F] = {
    val blockNoveltyDecoy = state.p2pNetworkConfig.blockNoveltyDecoy

    val blockRepMap =
      state.peersHandler.getHotPeers.map { case (id, peer) => id -> peer.blockRep * blockNoveltyDecoy }

    val noveltyRepMap =
      state.peersHandler.getHotPeers.map { case (id, peer) => id -> Math.max(peer.newRep - 1, 0) }

    val newPeersHandler =
      state.peersHandler.copyWithUpdatedReputation(blockRepMap = blockRepMap, noveltyRepMap = noveltyRepMap)

    state.copy(peersHandler = newPeersHandler)
  }

  private def hotToCold[F[_]: Async: Logger](
    thisActor: PeersManagerActor[F],
    state:     State[F]
  ): F[State[F]] = {
    val currentHotPeers: Map[HostId, Peer[F]] = state.peersHandler.getHotPeers
    val hotToCold = getHostsToCold(currentHotPeers, state.p2pNetworkConfig)

    for {
      _        <- Logger[F].infoIf(hotToCold.nonEmpty, s"Going to cold $hotToCold due of bad reputation")
      newState <- stopPeerActivity(thisActor, state, hotToCold, PeerState.Cold)
    } yield newState
  }

  private def warmToHot[F[_]: Async: Logger](
    thisActor: PeersManagerActor[F],
    state:     State[F]
  ): F[State[F]] = {
    val minimumHotConnections = state.p2pNetworkConfig.networkProperties.minimumHotConnections

    val lackHotPeersCount =
      minimumHotConnections - state.peersHandler.getHotPeers.count(_._2.remoteServerPort.isDefined)

    val warmToHot =
      state.warmToHotSelector.select(state.peersHandler.getWarmPeersWithActor, lackHotPeersCount)

    warmPeersToHot(thisActor, state, warmToHot)
  }

  private def getEligibleColdPeers[F[_]](state: State[F]): Map[HostId, Peer[F]] = {
    val closeTimeoutFirstDelayInMs = state.p2pNetworkConfig.networkProperties.closeTimeoutFirstDelayInMs
    val currentTimestamp = System.currentTimeMillis()

    state.peersHandler.getColdPeers.filter { case (_, host) =>
      val timestamps = host.closedTimestamps
      val lastClose = timestamps.lastOption.getOrElse(0L)
      val totalCloses = timestamps.size
      val nonEligibleWindow = totalCloses * totalCloses * closeTimeoutFirstDelayInMs
      currentTimestamp.toDouble >= (lastClose + nonEligibleWindow)
    }
  }

  private def coldToWarm[F[_]: Async: Parallel: Logger: DnsResolver](
    thisActor: PeersManagerActor[F],
    state:     State[F]
  ): F[State[F]] = {
    val maximumWarmConnection = state.p2pNetworkConfig.networkProperties.maximumWarmConnections
    val lackWarmPeersCount = maximumWarmConnection - state.peersHandler.getWarmPeersWithActor.size
    val eligibleColdPeers =
      getEligibleColdPeers(state).filter { case (_, peer) =>
        // we shall be able to establish connection or already have established connection
        peer.remoteServerPort.isDefined || peer.actorOpt.isDefined
      }
    val coldToWarm: Set[HostId] = state.coldToWarmSelector.select(eligibleColdPeers, lackWarmPeersCount)

    for {
      peersHandler <- state.peersHandler.moveToState(coldToWarm, PeerState.Warm, peerReleaseAction(thisActor))
      newState     <- state.copy(peersHandler = peersHandler).pure[F]
      _            <- checkConnection(newState, coldToWarm)
    } yield newState
  }

  private def checkConnection[F[_]: Async: Parallel: Logger: DnsResolver](
    state:        State[F],
    hostsToCheck: Set[HostId]
  ): F[Unit] = {
    val addressesToOpen: Seq[RemoteAddress] =
      hostsToCheck
        .flatMap(state.peersHandler.get)
        .filter(p => p.haveNoConnection && p.state.isActive)
        .flatMap(_.asRemoteAddress)
        .toSeq

    for {
      resolved      <- resolveHosts(addressesToOpen)
      nonLocalHosts <- resolved.filterNot(ra => state.thisHostIds.contains(ra.host)).pure[F]
      filtered      <- nonLocalHosts.filter(ra => state.peersHandler.haveNoActorForHost(ra.host)).pure[F]
      _             <- Logger[F].infoIf(filtered.nonEmpty, s"Going to open connection to next peers: $filtered")
      _             <- filtered.traverse(state.newPeerCreationAlgebra.requestNewPeerCreation)
    } yield ()
  }

  private def getHostsToCold[F[_]](
    currentHotPeers:  Map[HostId, Peer[F]],
    p2pNetworkConfig: P2PNetworkConfig
  ): Set[HostId] = {

    // TODO adjust performance reputation to avoid remote peer with best performance reputation but
    //  without actual application data providing
    val saveByPerformanceReputation =
      currentHotPeers.toSeq
        .map { case (id, peer) => id -> peer.perfRep }
        .sortBy(_._2)
        .takeRight(p2pNetworkConfig.networkProperties.minimumPerformanceReputationPeers)
        .map(_._1)
        .toSet

    val saveByBlockProviding =
      currentHotPeers.toSeq
        .map { case (id, peer) => id -> peer.blockRep }
        .sortBy(_._2)
        .takeRight(p2pNetworkConfig.networkProperties.minimumBlockProvidingReputationPeers)
        .map(_._1)
        .toSet

    val saveByNovelty = currentHotPeers
      .map { case (id, peer) => id -> peer.newRep }
      .filter(_._2 > 0)
      .keySet

    val saveByOverallReputation =
      currentHotPeers.filter { case (_, peer) =>
        val totalRep = peer.reputation
        totalRep >= p2pNetworkConfig.networkProperties.minimumRequiredReputation
      }.keySet

    val allKeptConnections =
      saveByNovelty ++ saveByBlockProviding ++ saveByPerformanceReputation ++ saveByOverallReputation

    currentHotPeers.keySet -- allKeptConnections
  }

  private def warmPeersToHot[F[_]: Async: Logger](
    thisActor: PeersManagerActor[F],
    state:     State[F],
    toHot:     Set[HostId]
  ): F[State[F]] =
    for {
      _            <- Logger[F].infoIf(toHot.nonEmpty, show"Going to hot next hosts: $toHot")
      updatedPeers <- state.peersHandler.moveToState(toHot, PeerState.Hot, peerReleaseAction(thisActor))
    } yield state.copy(peersHandler = updatedPeers)

}
// scalastyle:on number.of.methods
