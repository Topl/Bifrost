package co.topl.networking.fsnetwork

import cats.implicits._
import cats.effect.implicits._
import cats.effect.{Async, Resource}
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
import co.topl.networking.p2p.{DisconnectedPeer, PeerConnectionChange}
import co.topl.node.models.BlockBody
import fs2.concurrent.Topic
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import co.topl.typeclasses.implicits._
import co.topl.node.models.KnownHost
import co.topl.networking.fsnetwork.P2PShowInstances._

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
    client: BlockchainPeerClient[F] =>
      for {
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
      } yield ()
  }
}
