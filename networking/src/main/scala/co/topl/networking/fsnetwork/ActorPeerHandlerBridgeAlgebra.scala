package co.topl.networking.fsnetwork

import cats.effect.kernel.Concurrent
import cats.effect.{Async, Resource}
import cats.implicits._
import co.topl.algebras.Store
import co.topl.consensus.algebras._
import co.topl.eventtree.ParentChildTree
import co.topl.ledger.algebras._
import co.topl.networking.blockchain.{BlockchainPeerClient, BlockchainPeerHandlerAlgebra}
import co.topl.networking.fsnetwork.PeersManager.PeersManagerActor
import org.typelevel.log4cats.Logger
import co.topl.consensus.models.{BlockHeader, SlotData}
import co.topl.models.{Transaction, TypedIdentifier}
import co.topl.node.models.BlockBody

object ActorPeerHandlerBridgeAlgebra {

  def make[F[_]: Async: Logger](
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
    blockIdTree:                 ParentChildTree[F, TypedIdentifier]
  ): Resource[F, BlockchainPeerHandlerAlgebra[F]] = {
    val networkAlgebra = new NetworkAlgebraImpl[F]()
    val networkManager =
      NetworkManager.startNetwork[F](
        localChain,
        chainSelectionAlgebra,
        headerValidation,
        headerToBodyValidation,
        bodySyntaxValidation,
        bodySemanticValidation,
        bodyAuthorizationValidation,
        slotDataStore,
        headerStore,
        bodyStore,
        transactionStore,
        blockIdTree,
        networkAlgebra,
        List.empty
      )

    networkManager.map(makeAlgebra(_))
  }

  private def makeAlgebra[F[_]: Concurrent](peersManager: PeersManagerActor[F]): BlockchainPeerHandlerAlgebra[F] = {
    (client: BlockchainPeerClient[F]) =>
      for {
        hostId <- client.remotePeer.map(_.remoteAddress.host)
        _      <- peersManager.sendNoWait(PeersManager.Message.SetupPeer(hostId, client))
        _      <- peersManager.sendNoWait(PeersManager.Message.UpdatePeerStatus(hostId, PeerState.Hot))
      } yield ()
  }
}
