package co.topl.networking.fsnetwork

import cats.Parallel
import cats.effect.{Async, Resource}
import co.topl.algebras.{ClockAlgebra, Store}
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.validation.algebras.TransactionSyntaxVerifier
import co.topl.consensus.algebras._
import co.topl.consensus.models.{BlockHeader, BlockId, SlotData}
import co.topl.eventtree.ParentChildTree
import co.topl.ledger.algebras._
import co.topl.networking.blockchain.BlockchainPeerClient
import co.topl.networking.fsnetwork.BlockChecker.BlockCheckerActor
import co.topl.networking.fsnetwork.Notifier.NotifierActor
import co.topl.networking.fsnetwork.PeerActor.PeerActor
import co.topl.networking.fsnetwork.PeerBlockBodyFetcher.PeerBlockBodyFetcherActor
import co.topl.networking.fsnetwork.PeerBlockHeaderFetcher.PeerBlockHeaderFetcherActor
import co.topl.networking.fsnetwork.PeerMempoolTransactionSync.PeerMempoolTransactionSyncActor
import co.topl.networking.fsnetwork.PeersManager.PeersManagerActor
import co.topl.networking.fsnetwork.RequestsProxy.RequestsProxyActor
import co.topl.node.models.BlockBody
import com.github.benmanes.caffeine.cache.Caffeine
import org.typelevel.log4cats.Logger

trait NetworkAlgebra[F[_]] {

  def makePeerManger(
    thisHostId:                  HostId,
    networkAlgebra:              NetworkAlgebra[F],
    localChain:                  LocalChainAlgebra[F],
    slotDataStore:               Store[F, BlockId, SlotData],
    bodyStore:                   Store[F, BlockId, BlockBody],
    transactionStore:            Store[F, TransactionId, IoTransaction],
    blockIdTree:                 ParentChildTree[F, BlockId],
    mempool:                     MempoolAlgebra[F],
    headerToBodyValidation:      BlockHeaderToBodyValidationAlgebra[F],
    transactionSyntaxValidation: TransactionSyntaxVerifier[F],
    newPeerCreationAlgebra:      PeerCreationRequestAlgebra[F],
    p2pNetworkConfig:            P2PNetworkConfig,
    hotPeersUpdate:              Set[RemotePeer] => F[Unit],
    savePeersFunction:           Set[KnownRemotePeer] => F[Unit]
  ): Resource[F, PeersManagerActor[F]]

  def makeBlockChecker(
    requestsProxy:               RequestsProxyActor[F],
    localChain:                  LocalChainAlgebra[F],
    slotDataStore:               Store[F, BlockId, SlotData],
    headerStore:                 Store[F, BlockId, BlockHeader],
    bodyStore:                   Store[F, BlockId, BlockBody],
    headerValidation:            BlockHeaderValidationAlgebra[F],
    bodySyntaxValidation:        BodySyntaxValidationAlgebra[F],
    bodySemanticValidation:      BodySemanticValidationAlgebra[F],
    bodyAuthorizationValidation: BodyAuthorizationValidationAlgebra[F],
    chainSelectionAlgebra:       ChainSelectionAlgebra[F]
  ): Resource[F, BlockCheckerActor[F]]

  def makeRequestsProxy(
    peersManager: PeersManagerActor[F],
    headerStore:  Store[F, BlockId, BlockHeader],
    bodyStore:    Store[F, BlockId, BlockBody]
  ): Resource[F, RequestsProxyActor[F]]

  def makeNotifier(
    peersManager:     PeersManagerActor[F],
    p2pNetworkConfig: P2PNetworkConfig
  ): Resource[F, NotifierActor[F]]

  def makePeer(
    hostId:                      HostId,
    networkAlgebra:              NetworkAlgebra[F],
    client:                      BlockchainPeerClient[F],
    requestsProxy:               RequestsProxyActor[F],
    peersManager:                PeersManagerActor[F],
    localChain:                  LocalChainAlgebra[F],
    slotDataStore:               Store[F, BlockId, SlotData],
    bodyStore:                   Store[F, BlockId, BlockBody],
    transactionStore:            Store[F, TransactionId, IoTransaction],
    blockIdTree:                 ParentChildTree[F, BlockId],
    headerToBodyValidation:      BlockHeaderToBodyValidationAlgebra[F],
    transactionSyntaxValidation: TransactionSyntaxVerifier[F],
    mempool:                     MempoolAlgebra[F],
    commonAncestorF:             (BlockchainPeerClient[F], LocalChainAlgebra[F]) => F[BlockId]
  ): Resource[F, PeerActor[F]]

  def makePeerHeaderFetcher(
    hostId:          HostId,
    client:          BlockchainPeerClient[F],
    requestsProxy:   RequestsProxyActor[F],
    peersManager:    PeersManagerActor[F],
    localChain:      LocalChainAlgebra[F],
    slotDataStore:   Store[F, BlockId, SlotData],
    bodyStore:       Store[F, BlockId, BlockBody],
    blockIdTree:     ParentChildTree[F, BlockId],
    commonAncestorF: (BlockchainPeerClient[F], LocalChainAlgebra[F]) => F[BlockId]
  ): Resource[F, PeerBlockHeaderFetcherActor[F]]

  def makePeerBodyFetcher(
    hostId:                      HostId,
    client:                      BlockchainPeerClient[F],
    requestsProxy:               RequestsProxyActor[F],
    transactionStore:            Store[F, TransactionId, IoTransaction],
    headerToBodyValidation:      BlockHeaderToBodyValidationAlgebra[F],
    transactionSyntaxValidation: TransactionSyntaxVerifier[F]
  ): Resource[F, PeerBlockBodyFetcherActor[F]]

  def makeMempoolSyncFetcher(
    hostId:                      HostId,
    client:                      BlockchainPeerClient[F],
    transactionSyntaxValidation: TransactionSyntaxVerifier[F],
    transactionStore:            Store[F, TransactionId, IoTransaction],
    mempool:                     MempoolAlgebra[F],
    peersManager:                PeersManagerActor[F]
  ): Resource[F, PeerMempoolTransactionSyncActor[F]]
}

class NetworkAlgebraImpl[F[_]: Async: Parallel: Logger: DnsResolver: ReverseDnsResolver](clock: ClockAlgebra[F])
    extends NetworkAlgebra[F] {

  override def makePeerManger(
    thisHostId:                  HostId,
    networkAlgebra:              NetworkAlgebra[F],
    localChain:                  LocalChainAlgebra[F],
    slotDataStore:               Store[F, BlockId, SlotData],
    bodyStore:                   Store[F, BlockId, BlockBody],
    transactionStore:            Store[F, TransactionId, IoTransaction],
    blockIdTree:                 ParentChildTree[F, BlockId],
    mempool:                     MempoolAlgebra[F],
    headerToBodyValidation:      BlockHeaderToBodyValidationAlgebra[F],
    transactionSyntaxValidation: TransactionSyntaxVerifier[F],
    newPeerCreationAlgebra:      PeerCreationRequestAlgebra[F],
    p2pNetworkConfig:            P2PNetworkConfig,
    hotPeersUpdate:              Set[RemotePeer] => F[Unit],
    savePeersFunction:           Set[KnownRemotePeer] => F[Unit]
  ): Resource[F, PeersManagerActor[F]] = {
    val coldToWarm: SelectorColdToWarm[F] = new SemiRandomSelectorColdToWarm[F]()
    val warmToHot: SelectorWarmToHot[F] = new ReputationRandomBasedSelectorWarmToHot[F]()

    PeersManager.makeActor(
      thisHostId,
      networkAlgebra,
      localChain,
      slotDataStore,
      bodyStore,
      transactionStore,
      blockIdTree,
      mempool,
      headerToBodyValidation,
      transactionSyntaxValidation,
      newPeerCreationAlgebra,
      p2pNetworkConfig,
      hotPeersUpdate,
      savePeersFunction,
      coldToWarm,
      warmToHot,
      initialPeers = Map.empty[HostId, Peer[F]],
      blockSource = Caffeine.newBuilder.maximumSize(blockSourceCacheSize).build[BlockId, Set[HostId]]()
    )
  }

  override def makeBlockChecker(
    requestsProxy:               RequestsProxyActor[F],
    localChain:                  LocalChainAlgebra[F],
    slotDataStore:               Store[F, BlockId, SlotData],
    headerStore:                 Store[F, BlockId, BlockHeader],
    bodyStore:                   Store[F, BlockId, BlockBody],
    headerValidation:            BlockHeaderValidationAlgebra[F],
    bodySyntaxValidation:        BodySyntaxValidationAlgebra[F],
    bodySemanticValidation:      BodySemanticValidationAlgebra[F],
    bodyAuthorizationValidation: BodyAuthorizationValidationAlgebra[F],
    chainSelectionAlgebra:       ChainSelectionAlgebra[F]
  ): Resource[F, BlockCheckerActor[F]] =
    BlockChecker.makeActor(
      requestsProxy,
      localChain,
      slotDataStore,
      headerStore,
      bodyStore,
      headerValidation,
      bodySyntaxValidation,
      bodySemanticValidation,
      bodyAuthorizationValidation,
      chainSelectionAlgebra
    )

  override def makeRequestsProxy(
    peersManager: PeersManagerActor[F],
    headerStore:  Store[F, BlockId, BlockHeader],
    bodyStore:    Store[F, BlockId, BlockBody]
  ): Resource[F, RequestsProxyActor[F]] =
    RequestsProxy.makeActor(peersManager, headerStore, bodyStore)

  override def makeNotifier(
    peersManager:     PeersManagerActor[F],
    p2pNetworkConfig: P2PNetworkConfig
  ): Resource[F, NotifierActor[F]] =
    Notifier.makeActor(peersManager, p2pNetworkConfig)

  override def makePeer(
    hostId:                      HostId,
    networkAlgebra:              NetworkAlgebra[F],
    client:                      BlockchainPeerClient[F],
    requestsProxy:               RequestsProxyActor[F],
    peersManager:                PeersManagerActor[F],
    localChain:                  LocalChainAlgebra[F],
    slotDataStore:               Store[F, BlockId, SlotData],
    bodyStore:                   Store[F, BlockId, BlockBody],
    transactionStore:            Store[F, TransactionId, IoTransaction],
    blockIdTree:                 ParentChildTree[F, BlockId],
    headerToBodyValidation:      BlockHeaderToBodyValidationAlgebra[F],
    transactionSyntaxValidation: TransactionSyntaxVerifier[F],
    mempool:                     MempoolAlgebra[F],
    commonAncestorF:             (BlockchainPeerClient[F], LocalChainAlgebra[F]) => F[BlockId]
  ): Resource[F, PeerActor[F]] =
    PeerActor.makeActor(
      hostId,
      networkAlgebra,
      client,
      requestsProxy,
      peersManager,
      localChain,
      slotDataStore,
      bodyStore,
      transactionStore,
      blockIdTree,
      headerToBodyValidation,
      transactionSyntaxValidation,
      mempool,
      commonAncestorF
    )

  override def makePeerHeaderFetcher(
    hostId:          HostId,
    client:          BlockchainPeerClient[F],
    requestsProxy:   RequestsProxyActor[F],
    peersManager:    PeersManagerActor[F],
    localChain:      LocalChainAlgebra[F],
    slotDataStore:   Store[F, BlockId, SlotData],
    bodyStore:       Store[F, BlockId, BlockBody],
    blockIdTree:     ParentChildTree[F, BlockId],
    commonAncestorF: (BlockchainPeerClient[F], LocalChainAlgebra[F]) => F[BlockId]
  ): Resource[F, PeerBlockHeaderFetcherActor[F]] =
    PeerBlockHeaderFetcher.makeActor(
      hostId,
      client,
      requestsProxy,
      peersManager,
      localChain,
      slotDataStore,
      bodyStore,
      blockIdTree,
      clock,
      commonAncestorF
    )

  override def makePeerBodyFetcher(
    hostId:                      HostId,
    client:                      BlockchainPeerClient[F],
    requestsProxy:               RequestsProxyActor[F],
    transactionStore:            Store[F, TransactionId, IoTransaction],
    headerToBodyValidation:      BlockHeaderToBodyValidationAlgebra[F],
    transactionSyntaxValidation: TransactionSyntaxVerifier[F]
  ): Resource[F, PeerBlockBodyFetcherActor[F]] =
    PeerBlockBodyFetcher.makeActor(
      hostId,
      client,
      requestsProxy,
      transactionStore,
      headerToBodyValidation,
      transactionSyntaxValidation
    )

  override def makeMempoolSyncFetcher(
    hostId:                      HostId,
    client:                      BlockchainPeerClient[F],
    transactionSyntaxValidation: TransactionSyntaxVerifier[F],
    transactionStore:            Store[F, TransactionId, IoTransaction],
    mempool:                     MempoolAlgebra[F],
    peersManager:                PeersManagerActor[F]
  ): Resource[F, PeerMempoolTransactionSyncActor[F]] =
    PeerMempoolTransactionSync.makeActor(
      hostId,
      client,
      transactionSyntaxValidation,
      transactionStore,
      mempool,
      peersManager
    )
}
