package co.topl.networking.fsnetwork

import cats.effect.Async
import cats.effect.Resource
import cats.effect.kernel.Concurrent
import cats.effect.implicits._
import cats.implicits._
import co.topl.algebras.Store
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.config.ApplicationConfig.Bifrost.NetworkProperties
import co.topl.consensus.algebras._
import co.topl.consensus.models.BlockHeader
import co.topl.consensus.models.BlockId
import co.topl.consensus.models.SlotData
import co.topl.eventtree.ParentChildTree
import co.topl.ledger.algebras._
import co.topl.networking.blockchain.BlockchainPeerClient
import co.topl.networking.blockchain.BlockchainPeerHandlerAlgebra
import co.topl.networking.fsnetwork.PeersManager.PeersManagerActor
import co.topl.node.models.BlockBody
import org.typelevel.log4cats.Logger

object ActorPeerHandlerBridgeAlgebra {

  def make[F[_]: Async: Logger](
    localChain:                  LocalChainAlgebra[F],
    chainSelectionAlgebra:       ChainSelectionAlgebra[F, SlotData],
    headerValidation:            BlockHeaderValidationAlgebra[F],
    headerToBodyValidation:      BlockHeaderToBodyValidationAlgebra[F],
    bodySyntaxValidation:        BodySyntaxValidationAlgebra[F],
    bodySemanticValidation:      BodySemanticValidationAlgebra[F],
    bodyAuthorizationValidation: BodyAuthorizationValidationAlgebra[F],
    slotDataStore:               Store[F, BlockId, SlotData],
    headerStore:                 Store[F, BlockId, BlockHeader],
    bodyStore:                   Store[F, BlockId, BlockBody],
    transactionStore:            Store[F, TransactionId, IoTransaction],
    blockIdTree:                 ParentChildTree[F, BlockId],
    networkProperties:           NetworkProperties
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
        List.empty,
        networkProperties
      )

    networkManager.map(makeAlgebra(_))
  }

  private def makeAlgebra[F[_]: Concurrent](peersManager: PeersManagerActor[F]): BlockchainPeerHandlerAlgebra[F] = {
    (client: BlockchainPeerClient[F]) =>
      for {
        hostId <- client.remotePeer.map(_.remoteAddress.host).toResource
        // TODO: Resource.onFinalize send "DestroyPeer" message?
        _ <- peersManager.sendNoWait(PeersManager.Message.SetupPeer(hostId, client)).toResource
        _ <- peersManager.sendNoWait(PeersManager.Message.UpdatePeerStatus(hostId, PeerState.Hot)).toResource
        _ <- peersManager.sendNoWait(PeersManager.Message.GetCurrentTip(hostId)).toResource
      } yield ()
  }
}
