package co.topl.networking.fsnetwork

import cats.effect.kernel.Resource
import co.topl.algebras.Store
import co.topl.consensus.algebras._
import co.topl.consensus.models.{BlockHeader, SlotData}
import co.topl.eventtree.ParentChildTree
import co.topl.ledger.algebras._
import co.topl.models.{Transaction, TypedIdentifier}
import co.topl.networking.fsnetwork.PeersManager.PeersManagerActor
import co.topl.node.models.BlockBody
import org.typelevel.log4cats.Logger

object NetworkManager {

  def startNetwork[F[_]: Logger](
    localChain:                  LocalChainAlgebra[F],
    chainSelectionAlgebra:       ChainSelectionAlgebra[F, SlotData],
    headerValidation:            BlockHeaderValidationAlgebra[F],
    headerToBodyValidation:      BlockHeaderToBodyValidationAlgebra[F],
    bodySyntaxValidation:        BodySyntaxValidationAlgebra[F],
    bodySemanticValidation:      BodySemanticValidationAlgebra[F],
    bodyAuthorizationValidation: BodyAuthorizationValidationAlgebra[F],
    slotDataStore:               Store[F, TypedIdentifier, SlotData],
    headerStore:                 Store[F, TypedIdentifier, BlockHeader],
    bodyStore:                   Store[F, TypedIdentifier, BlockBody],
    transactionStore:            Store[F, TypedIdentifier, Transaction],
    blockIdTree:                 ParentChildTree[F, TypedIdentifier],
    networkAlgebra:              NetworkAlgebra[F],
    initialHosts:                List[HostId]
  ): Resource[F, PeersManagerActor[F]] =
    for {
      _ <- Resource.liftK(Logger[F].info("Start actors network"))
      peerManager <- networkAlgebra.makePeerManger(
        networkAlgebra,
        localChain,
        slotDataStore,
        transactionStore,
        blockIdTree
      )
      reputationAggregator <- networkAlgebra.makeReputationAggregation(peerManager)
      blocksChecker <- networkAlgebra.makeBlockChecker(
        reputationAggregator,
        peerManager,
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

      _ <- Resource.liftK(peerManager.sendNoWait(PeersManager.Message.SetupReputationAggregator(reputationAggregator)))
      _ <- Resource.liftK(peerManager.sendNoWait(PeersManager.Message.SetupBlockChecker(blocksChecker)))
      // TODO send initial host list to peer manager
    } yield peerManager
}
