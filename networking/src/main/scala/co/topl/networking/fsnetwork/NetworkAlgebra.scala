package co.topl.networking.fsnetwork

import cats.effect.{Async, Resource}
import co.topl.algebras.Store
import co.topl.consensus.algebras._
import co.topl.eventtree.ParentChildTree
import co.topl.ledger.algebras._
import co.topl.models._
import co.topl.networking.blockchain.BlockchainPeerClient
import co.topl.networking.fsnetwork.BlockChecker.BlockCheckerActor
import co.topl.networking.fsnetwork.PeerActor.PeerActor
import co.topl.networking.fsnetwork.PeersManager.PeersManagerActor
import co.topl.networking.fsnetwork.ReputationAggregator.ReputationAggregatorActor
import org.typelevel.log4cats.Logger

import scala.reflect.ClassTag

trait NetworkAlgebra[F[_]] {

  def makePeerManger(
    networkAlgebra:   NetworkAlgebra[F],
    localChain:       LocalChainAlgebra[F],
    slotDataStore:    Store[F, TypedIdentifier, SlotData],
    transactionStore: Store[F, TypedIdentifier, Transaction],
    blockIdTree:      ParentChildTree[F, TypedIdentifier]
  ): Resource[F, PeersManagerActor[F]]

  def makePeer(
                hostId:               HostId,
                client:               BlockchainPeerClient[F],
                reputationAggregator: ReputationAggregatorActor[F],
                blockHeaderChecker:   BlockCheckerActor[F],
                localChain:           LocalChainAlgebra[F],
                slotDataStore:        Store[F, TypedIdentifier, SlotData],
                transactionStore:     Store[F, TypedIdentifier, Transaction],
                blockIdTree:          ParentChildTree[F, TypedIdentifier]
  ): Resource[F, PeerActor[F]]

  def makeReputationAggregation(peersManager: PeersManagerActor[F]): Resource[F, ReputationAggregatorActor[F]]

  def makeBlockChecker(
    reputationAggregator:        ReputationAggregatorActor[F],
    peersManager:                PeersManagerActor[F],
    localChain:                  LocalChainAlgebra[F],
    slotDataStore:               Store[F, TypedIdentifier, SlotData],
    headerStore:                 Store[F, TypedIdentifier, BlockHeader],
    bodyStore:                   Store[F, TypedIdentifier, BlockBody],
    headerValidation:            BlockHeaderValidationAlgebra[F],
    headerToBodyValidation:      BlockHeaderToBodyValidationAlgebra[F],
    bodySyntaxValidation:        BodySyntaxValidationAlgebra[F],
    bodySemanticValidation:      BodySemanticValidationAlgebra[F],
    bodyAuthorizationValidation: BodyAuthorizationValidationAlgebra[F],
    chainSelectionAlgebra:       ChainSelectionAlgebra[F, SlotData]
  ): Resource[F, BlockCheckerActor[F]]

}

class NetworkAlgebraImpl[F[_]: Async: Logger] extends NetworkAlgebra[F] {

  override def makePeerManger(
    networkAlgebra:   NetworkAlgebra[F],
    localChain:       LocalChainAlgebra[F],
    slotDataStore:    Store[F, TypedIdentifier, SlotData],
    transactionStore: Store[F, TypedIdentifier, Transaction],
    blockIdTree:      ParentChildTree[F, TypedIdentifier]
  ): Resource[F, PeersManagerActor[F]] =
    PeersManager.makeActor(
      networkAlgebra,
      localChain,
      slotDataStore,
      transactionStore,
      blockIdTree
    )

  override def makePeer(
                         hostId:               HostId,
                         client:               BlockchainPeerClient[F],
                         reputationAggregator: ReputationAggregatorActor[F],
                         blockHeaderChecker:   BlockCheckerActor[F],
                         localChain:           LocalChainAlgebra[F],
                         slotDataStore:        Store[F, TypedIdentifier, SlotData],
                         transactionStore:     Store[F, TypedIdentifier, Transaction],
                         blockIdTree:          ParentChildTree[F, TypedIdentifier]
  ): Resource[F, PeerActor[F]] =
    PeerActor.makeActor(
      hostId,
      client,
      reputationAggregator,
      blockHeaderChecker,
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
    localChain:                  LocalChainAlgebra[F],
    slotDataStore:               Store[F, TypedIdentifier, SlotData],
    headerStore:                 Store[F, TypedIdentifier, BlockHeader],
    bodyStore:                   Store[F, TypedIdentifier, BlockBody],
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
}
