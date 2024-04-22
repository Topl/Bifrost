package co.topl.networking.fsnetwork

import cats.effect.implicits._
import cats.effect.kernel.Sync
import cats.effect.{Async, Deferred, Resource}
import cats.implicits._
import cats.{Monad, MonadThrow}
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
import co.topl.networking.p2p.{ConnectedPeer, DisconnectedPeer, PeerConnectionChange}
import co.topl.node.models._
import co.topl.typeclasses.implicits._
import P2PShowInstances._
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
    remotePeerStore:             Store[F, Unit, Seq[KnownRemotePeer]],
    blockIdTree:                 ParentChildTree[F, BlockId],
    blockHeights:                EventSourcedState[F, Long => F[Option[BlockId]], BlockId],
    mempool:                     MempoolAlgebra[F],
    networkProperties:           NetworkProperties,
    clockAlgebra:                ClockAlgebra[F],
    remotePeers:                 Seq[DisconnectedPeer],
    peersStatusChangesTopic:     Topic[F, PeerConnectionChange],
    addRemotePeer:               DisconnectedPeer => F[Unit],
    hotPeersUpdate:              Set[RemotePeer] => F[Unit]
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
        remotePeers,
        networkProperties,
        clockAlgebra,
        PeerCreationRequestAlgebra(addRemotePeer),
        peersStatusChangesTopic,
        hotPeersUpdate
      )

    networkManager.map(pm => makeAlgebra(pm))
  }

  private def makeAlgebra[F[_]: Async: Logger](peersManager: PeersManagerActor[F]): BlockchainPeerHandlerAlgebra[F] = {
    underlyingClient: BlockchainPeerClient[F] =>
      for {
        // A callback/Deferred that is signaled by the "closeConnection()" call
        closeDeferred <- Deferred[F, Unit].toResource
        client = new BlockchainPeerClientCloseHook[F](underlyingClient, Async[F].defer(closeDeferred.complete(()).void))
        remoteId <- client.remotePeer.p2pVK.pure[F].toResource
        remotePeerOpt <- client.remotePeerAsServer.handleErrorWith { e =>
          Logger[F].error(show"Failed to get remote peer as server from $remoteId due ${e.toString}") >>
          Option.empty[KnownHost].pure[F]
        }.toResource

        peerAsServer = remotePeerOpt match {
          case Some(kh) if kh.id == remoteId => kh.some
          case _                             => None
        }

        _ <-
          if (remotePeerOpt.isDefined && peerAsServer.isEmpty)
            Logger[F].warn(show"Remote peer $remoteId provide bad server info $remotePeerOpt").toResource
          else Resource.pure[F, Unit](())
        _ <- peersManager.sendNoWait(PeersManager.Message.OpenedPeerConnection(client, peerAsServer)).toResource
        _ <- closeDeferred.get.toResource
        _ <- Logger[F].info(show"Remote peer $remoteId complete").toResource
      } yield ()
  }
}

/**
 * Wraps a BlockchainPeerClient instance with an extra hook on the "closeConnection" method
 * @param underlying an underlying client instance
 * @param onClose a hook to be invoked when closeConnection is called
 */
class BlockchainPeerClientCloseHook[F[_]: Monad](underlying: BlockchainPeerClient[F], onClose: F[Unit])
    extends BlockchainPeerClient[F] {

  override def remotePeer: ConnectedPeer = underlying.remotePeer

  override def remotePeerAsServer: F[Option[KnownHost]] = underlying.remotePeerAsServer

  override def remotePeerAdoptions: F[fs2.Stream[F, BlockId]] = underlying.remotePeerAdoptions

  override def remoteTransactionNotifications: F[fs2.Stream[F, TransactionId]] =
    underlying.remoteTransactionNotifications

  override def getRemoteBlockIdAtDepth(depth: Long): F[Option[BlockId]] = underlying.getRemoteBlockIdAtDepth(depth)

  override def getRemoteSlotData(id: BlockId): F[Option[SlotData]] = underlying.getRemoteSlotData(id)

  override def getRemoteHeader(id: BlockId): F[Option[BlockHeader]] = underlying.getRemoteHeader(id)

  override def getRemoteBody(id: BlockId): F[Option[BlockBody]] = underlying.getRemoteBody(id)

  override def getRemoteTransaction(id: TransactionId): F[Option[IoTransaction]] = underlying.getRemoteTransaction(id)

  override def getRemoteBlockIdAtHeight(height: Long): F[Option[BlockId]] = underlying.getRemoteBlockIdAtHeight(height)

  override def getRemoteKnownHosts(request: CurrentKnownHostsReq): F[Option[CurrentKnownHostsRes]] =
    underlying.getRemoteKnownHosts(request)

  override def getPongMessage(request: PingMessage): F[Option[PongMessage]] = underlying.getPongMessage(request)

  override def notifyAboutThisNetworkLevel(networkLevel: Boolean): F[Unit] =
    underlying.notifyAboutThisNetworkLevel(networkLevel)

  override def closeConnection(): F[Unit] = onClose >> underlying.closeConnection()

  override def getRemoteTransactionOrError[E <: Throwable](id: TransactionId, error: => E)(implicit
    MonadThrow: MonadThrow[F]
  ): F[IoTransaction] = underlying.getRemoteTransactionOrError(id, error)

  override def findCommonAncestor(
    getLocalBlockIdAtHeight: Long => F[BlockId],
    currentHeight:           () => F[Long]
  )(implicit syncF: Sync[F], loggerF: Logger[F]): F[BlockId] =
    underlying.findCommonAncestor(getLocalBlockIdAtHeight, currentHeight)
}
