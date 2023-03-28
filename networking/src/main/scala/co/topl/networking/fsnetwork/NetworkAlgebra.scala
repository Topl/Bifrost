package co.topl.networking.fsnetwork

import cats.effect.Async
import cats.effect.Resource
import co.topl.algebras.Store
import co.topl.brambl.models.Identifier
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.consensus.algebras._
import co.topl.consensus.models.BlockId
import co.topl.consensus.models.BlockHeader
import co.topl.consensus.models.SlotData
import co.topl.eventtree.ParentChildTree
import co.topl.ledger.algebras._
import co.topl.networking.fsnetwork.BlockChecker.BlockCheckerActor
import co.topl.networking.fsnetwork.PeersManager.PeersManagerActor
import co.topl.networking.fsnetwork.ReputationAggregator.ReputationAggregatorActor
import co.topl.networking.fsnetwork.RequestsProxy.RequestsProxyActor
import co.topl.node.models.BlockBody
import org.typelevel.log4cats.Logger

trait NetworkAlgebra[F[_]] {

  def makePeerManger(
    networkAlgebra:   NetworkAlgebra[F],
    localChain:       LocalChainAlgebra[F],
    slotDataStore:    Store[F, BlockId, SlotData],
    transactionStore: Store[F, Identifier.IoTransaction32, IoTransaction],
    blockIdTree:      ParentChildTree[F, BlockId]
  ): Resource[F, PeersManagerActor[F]]

  def makeReputationAggregation(peersManager: PeersManagerActor[F]): Resource[F, ReputationAggregatorActor[F]]

  def makeBlockChecker(
    reputationAggregator:        ReputationAggregatorActor[F],
    peersManager:                PeersManagerActor[F],
    requestsProxy:               RequestsProxyActor[F],
    localChain:                  LocalChainAlgebra[F],
    slotDataStore:               Store[F, BlockId, SlotData],
    headerStore:                 Store[F, BlockId, BlockHeader],
    bodyStore:                   Store[F, BlockId, BlockBody],
    headerValidation:            BlockHeaderValidationAlgebra[F],
    headerToBodyValidation:      BlockHeaderToBodyValidationAlgebra[F],
    bodySyntaxValidation:        BodySyntaxValidationAlgebra[F],
    bodySemanticValidation:      BodySemanticValidationAlgebra[F],
    bodyAuthorizationValidation: BodyAuthorizationValidationAlgebra[F],
    chainSelectionAlgebra:       ChainSelectionAlgebra[F, SlotData]
  ): Resource[F, BlockCheckerActor[F]]

  def makeRequestsProxy(
    reputationAggregator: ReputationAggregatorActor[F],
    peersManager:         PeersManagerActor[F],
    headerStore:          Store[F, BlockId, BlockHeader],
    bodyStore:            Store[F, BlockId, BlockBody]
  ): Resource[F, RequestsProxyActor[F]]
}

class NetworkAlgebraImpl[F[_]: Async: Logger] extends NetworkAlgebra[F] {

  override def makePeerManger(
    networkAlgebra:   NetworkAlgebra[F],
    localChain:       LocalChainAlgebra[F],
    slotDataStore:    Store[F, BlockId, SlotData],
    transactionStore: Store[F, Identifier.IoTransaction32, IoTransaction],
    blockIdTree:      ParentChildTree[F, BlockId]
  ): Resource[F, PeersManagerActor[F]] =
    PeersManager.makeActor(
      networkAlgebra,
      localChain,
      slotDataStore,
      transactionStore,
      blockIdTree
    )

  override def makeReputationAggregation(
    peersManager: PeersManagerActor[F]
  ): Resource[F, ReputationAggregatorActor[F]] =
    ReputationAggregator.makeActor(peersManager)

  override def makeBlockChecker(
    reputationAggregator:        ReputationAggregatorActor[F],
    peersManager:                PeersManagerActor[F],
    requestsProxy:               RequestsProxyActor[F],
    localChain:                  LocalChainAlgebra[F],
    slotDataStore:               Store[F, BlockId, SlotData],
    headerStore:                 Store[F, BlockId, BlockHeader],
    bodyStore:                   Store[F, BlockId, BlockBody],
    headerValidation:            BlockHeaderValidationAlgebra[F],
    headerToBodyValidation:      BlockHeaderToBodyValidationAlgebra[F],
    bodySyntaxValidation:        BodySyntaxValidationAlgebra[F],
    bodySemanticValidation:      BodySemanticValidationAlgebra[F],
    bodyAuthorizationValidation: BodyAuthorizationValidationAlgebra[F],
    chainSelectionAlgebra:       ChainSelectionAlgebra[F, SlotData]
  ): Resource[F, BlockCheckerActor[F]] =
    BlockChecker.makeActor(
      reputationAggregator,
      peersManager,
      requestsProxy,
      localChain,
      slotDataStore,
      headerStore,
      bodyStore,
      headerValidation,
      headerToBodyValidation,
      bodySyntaxValidation,
      bodySemanticValidation,
      bodyAuthorizationValidation,
      chainSelectionAlgebra
    )

  def makeRequestsProxy(
    reputationAggregator: ReputationAggregatorActor[F],
    peersManager:         PeersManagerActor[F],
    headerStore:          Store[F, BlockId, BlockHeader],
    bodyStore:            Store[F, BlockId, BlockBody]
  ): Resource[F, RequestsProxyActor[F]] =
    RequestsProxy.makeActor(reputationAggregator, peersManager, headerStore, bodyStore)
}
