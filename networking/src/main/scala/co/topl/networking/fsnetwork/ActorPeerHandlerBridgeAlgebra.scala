package co.topl.networking.fsnetwork

import cats.effect.implicits._
import cats.effect.kernel.Concurrent
import cats.effect.{Async, Resource}
import cats.implicits._
import co.topl.algebras.{ClockAlgebra, Store}
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.validation.algebras.TransactionSyntaxVerifier
import co.topl.config.ApplicationConfig.Bifrost.NetworkProperties
import co.topl.consensus.algebras._
import co.topl.consensus.models.{BlockHeader, BlockId, SlotData}
import co.topl.eventtree.{EventSourcedState, ParentChildTree}
import co.topl.ledger.algebras._
import co.topl.networking.blockchain.{BlockchainPeerClient, BlockchainPeerHandlerAlgebra}
import co.topl.networking.fsnetwork.PeersManager.PeersManagerActor
import co.topl.networking.p2p.{DisconnectedPeer, PeerConnectionChange, RemoteAddress}
import co.topl.node.models.BlockBody
import fs2.concurrent.Topic
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

object ActorPeerHandlerBridgeAlgebra {

  def make[F[_]: Async: DnsResolver: ReverseDnsResolver](
    thisHostId:                  HostId,
    localChain:                  LocalChainAlgebra[F],
    chainSelectionAlgebra:       ChainSelectionAlgebra[F, SlotData],
    headerValidation:            BlockHeaderValidationAlgebra[F],
    headerToBodyValidation:      BlockHeaderToBodyValidationAlgebra[F],
    transactionSyntaxValidation: TransactionSyntaxVerifier[F],
    bodySyntaxValidation:        BodySyntaxValidationAlgebra[F],
    bodySemanticValidation:      BodySemanticValidationAlgebra[F],
    bodyAuthorizationValidation: BodyAuthorizationValidationAlgebra[F],
    slotDataStore:               Store[F, BlockId, SlotData],
    headerStore:                 Store[F, BlockId, BlockHeader],
    bodyStore:                   Store[F, BlockId, BlockBody],
    transactionStore:            Store[F, TransactionId, IoTransaction],
    remotePeerStore:             Store[F, Unit, Seq[RemotePeer]],
    blockIdTree:                 ParentChildTree[F, BlockId],
    blockHeights:                EventSourcedState[F, Long => F[Option[BlockId]], BlockId],
    mempool:                     MempoolAlgebra[F],
    networkProperties:           NetworkProperties,
    clockAlgebra:                ClockAlgebra[F],
    remotePeers:                 Seq[DisconnectedPeer],
    peersStatusChangesTopic:     Topic[F, PeerConnectionChange],
    addRemotePeer:               DisconnectedPeer => F[Unit],
    hotPeersUpdate:              Set[RemoteAddress] => F[Unit]
  ): Resource[F, BlockchainPeerHandlerAlgebra[F]] = {
    implicit val logger: Logger[F] = Slf4jLogger.getLoggerFromName("Bifrost.P2P")

    val networkAlgebra = new NetworkAlgebraImpl[F](clockAlgebra)
    val networkManager =
      NetworkManager.startNetwork[F](
        thisHostId,
        localChain,
        chainSelectionAlgebra,
        headerValidation,
        headerToBodyValidation,
        transactionSyntaxValidation,
        bodySyntaxValidation,
        bodySemanticValidation,
        bodyAuthorizationValidation,
        slotDataStore,
        headerStore,
        bodyStore,
        transactionStore,
        remotePeerStore,
        blockIdTree,
        blockHeights,
        mempool,
        networkAlgebra,
        remotePeers.map(_.remoteAddress),
        networkProperties,
        clockAlgebra,
        PeerCreationRequestAlgebra(addRemotePeer),
        peersStatusChangesTopic,
        hotPeersUpdate
      )

    networkManager.map(makeAlgebra(_))
  }

  private def makeAlgebra[F[_]: Concurrent](peersManager: PeersManagerActor[F]): BlockchainPeerHandlerAlgebra[F] = {
    (client: BlockchainPeerClient[F]) =>
      for {
        hostId <- client.remotePeer.map(_.remoteAddress).toResource
        _      <- peersManager.sendNoWait(PeersManager.Message.OpenedPeerConnection(hostId, client)).toResource
      } yield ()
  }
}
