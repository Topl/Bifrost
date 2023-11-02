package co.topl.networking.fsnetwork

import cats.Applicative
import cats.data.NonEmptyChain
import cats.effect.Async
import cats.effect.implicits.genSpawnOps
import cats.effect.kernel.{Outcome, Resource}
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
import co.topl.networking.fsnetwork.PeersManager.PeersManagerActor
import co.topl.networking.p2p.{PeerConnectionChange, PeerConnectionChanges, RemoteAddress}
import co.topl.node.models.BlockBody
import fs2.concurrent.Topic
import org.typelevel.log4cats.Logger

object NetworkManager {

  def startNetwork[F[_]: Async: Logger](
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
    networkAlgebra:              NetworkAlgebra[F],
    initialHosts:                Seq[RemoteAddress],
    networkProperties:           NetworkProperties,
    clock:                       ClockAlgebra[F],
    addRemotePeerAlgebra:        PeerCreationRequestAlgebra[F],
    peersStatusChangesTopic:     Topic[F, PeerConnectionChange],
    hotPeersUpdate:              Set[RemoteAddress] => F[Unit]
  ): Resource[F, PeersManagerActor[F]] =
    for {
      _            <- Resource.liftK(Logger[F].info(s"Start actors network with list of peers: $initialHosts"))
      slotDuration <- Resource.liftK(clock.slotLength)
      p2pNetworkConfig = P2PNetworkConfig(networkProperties, slotDuration)

      peersFromStorage <- Resource.liftK(remotePeerStore.get(()).map(_.getOrElse(Seq.empty)))
      _                <- Resource.liftK(Logger[F].info(s"Loaded from storage next known hosts: $peersFromStorage"))
      peerManager <- networkAlgebra.makePeerManger(
        thisHostId,
        networkAlgebra,
        localChain,
        slotDataStore,
        transactionStore,
        blockIdTree,
        blockHeights,
        mempool,
        headerToBodyValidation,
        transactionSyntaxValidation,
        addRemotePeerAlgebra,
        p2pNetworkConfig,
        hotPeersUpdate,
        buildSaveRemotePeersFunction(remotePeerStore)
      )

      requestsProxy <- networkAlgebra.makeRequestsProxy(peerManager, headerStore, bodyStore)
      blocksChecker <- networkAlgebra.makeBlockChecker(
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

      _ <- Resource.liftK(requestsProxy.sendNoWait(RequestsProxy.Message.SetupBlockChecker(blocksChecker)))
      _ <- Resource.liftK(peerManager.sendNoWait(PeersManager.Message.SetupBlockChecker(blocksChecker)))
      _ <- Resource.liftK(peerManager.sendNoWait(PeersManager.Message.SetupRequestsProxy(requestsProxy)))

      _ <- Resource.liftK(
        NonEmptyChain
          .fromSeq(mergeRemotePeersAndRemoteAddress(peersFromStorage, initialHosts))
          .map { initialPeers =>
            peerManager.sendNoWait(PeersManager.Message.AddKnownPeers(initialPeers))
          }
          .getOrElse(Logger[F].error(show"No know hosts are set during node startup"))
      )

      notifier <- networkAlgebra.makeNotifier(peerManager, p2pNetworkConfig)
      _        <- Resource.liftK(notifier.sendNoWait(Notifier.Message.StartNotifications))

      _ <- startPeersStatusNotifier(peerManager, peersStatusChangesTopic)
    } yield peerManager

  private def startPeersStatusNotifier[F[_]: Async: Logger](
    peersManager: PeersManagerActor[F],
    topic:        Topic[F, PeerConnectionChange]
  ): Resource[F, F[Outcome[F, Throwable, Unit]]] =
    topic.subscribeUnbounded
      .evalTap {
        case PeerConnectionChanges.InboundConnectionInitializing(_, _) => Applicative[F].unit
        case PeerConnectionChanges.OutboundConnectionInitializing(_)   => Applicative[F].unit
        case PeerConnectionChanges.ConnectionEstablished(_, localAddress) =>
          peersManager.sendNoWait(PeersManager.Message.UpdateThisPeerAddress(localAddress))
        case PeerConnectionChanges.ConnectionClosed(connectedPeer, _) =>
          Logger[F].info(s"Remote peer ${connectedPeer.remoteAddress} closing had been detected") >>
          peersManager.sendNoWait(PeersManager.Message.ClosePeer(connectedPeer.remoteAddress.host))
        case PeerConnectionChanges.RemotePeerApplicationLevel(connectedPeer, appLevel) =>
          val host = connectedPeer.remoteAddress.host
          peersManager.sendNoWait(PeersManager.Message.RemotePeerNetworkLevel(host, appLevel))
      }
      .compile
      .drain
      .background

  private def buildSaveRemotePeersFunction[F[_]: Async: Logger](
    remotePeersStore: Store[F, Unit, Seq[RemotePeer]]
  ): Set[RemotePeer] => F[Unit] = { peers: Set[RemotePeer] =>
    Logger[F].info(s"Going to save known hosts $peers to local data storage") >>
    remotePeersStore.put((), peers.toList)
  }

  // peers represented as Remote address could be present in remote peers as well with some reputation
  private def mergeRemotePeersAndRemoteAddress(
    remotePeers:   Seq[RemotePeer],
    remoteAddress: Seq[RemoteAddress]
  ): Seq[RemotePeer] = {
    val remoteAddressMap = remoteAddress.map(ra => ra -> RemotePeer(ra, 0.0, 0.0)).toMap
    val remotePeersMap = remotePeers.map(p => p.address -> p).toMap

    (remoteAddressMap ++ remotePeersMap).values.toSeq
  }
}
