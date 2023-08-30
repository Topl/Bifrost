package co.topl.networking.fsnetwork

import cats.effect.implicits._
import cats.effect.kernel.Concurrent
import cats.effect.{Async, Resource}
import cats.implicits._
import co.topl.algebras.{ClockAlgebra, Store}
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.config.ApplicationConfig.Bifrost.NetworkProperties
import co.topl.consensus.algebras._
import co.topl.consensus.models.{BlockHeader, BlockId, SlotData}
import co.topl.eventtree.ParentChildTree
import co.topl.ledger.algebras._
import co.topl.networking.blockchain.{BlockchainPeerClient, BlockchainPeerHandlerAlgebra}
import co.topl.networking.fsnetwork.PeersManager.PeersManagerActor
import co.topl.networking.p2p.{ConnectedPeer, DisconnectedPeer, RemoteAddress}
import co.topl.node.models.{BlockBody, KnownHost}
import fs2.Stream
import org.typelevel.log4cats.Logger

object ActorPeerHandlerBridgeAlgebra {

  def make[F[_]: Async: Logger: DnsResolver](
    thisHostId:                  HostId,
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
    knownHostsStore:             Store[F, Unit, Seq[KnownHost]],
    blockIdTree:                 ParentChildTree[F, BlockId],
    networkProperties:           NetworkProperties,
    clockAlgebra:                ClockAlgebra[F],
    remotePeers:                 List[DisconnectedPeer],
    closedPeers:                 Stream[F, ConnectedPeer],
    addRemotePeer:               DisconnectedPeer => F[Unit],
    hotPeersUpdate:              Set[RemoteAddress] => F[Unit]
  ): Resource[F, BlockchainPeerHandlerAlgebra[F]] = {

    val networkAlgebra = new NetworkAlgebraImpl[F](clockAlgebra)
    val networkManager =
      NetworkManager.startNetwork[F](
        thisHostId,
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
        knownHostsStore,
        blockIdTree,
        networkAlgebra,
        remotePeers.map(_.remoteAddress),
        networkProperties,
        clockAlgebra,
        PeerCreationRequestAlgebra(addRemotePeer),
        closedPeers,
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
