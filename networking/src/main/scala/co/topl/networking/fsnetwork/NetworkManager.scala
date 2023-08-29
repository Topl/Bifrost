package co.topl.networking.fsnetwork

import cats.data.NonEmptyChain
import cats.effect.Async
import cats.effect.implicits.genSpawnOps
import cats.effect.kernel.{Outcome, Resource}
import cats.implicits._
import co.topl.algebras.{ClockAlgebra, Store}
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.config.ApplicationConfig.Bifrost.NetworkProperties
import co.topl.consensus.algebras._
import co.topl.consensus.models.{BlockHeader, BlockId, SlotData}
import co.topl.eventtree.ParentChildTree
import co.topl.ledger.algebras._
import co.topl.networking.KnownHostOps
import co.topl.networking.fsnetwork.PeersManager.PeersManagerActor
import co.topl.networking.p2p.{ConnectedPeer, RemoteAddress}
import co.topl.node.models.{BlockBody, KnownHost}
import fs2.Stream
import org.typelevel.log4cats.Logger

object NetworkManager {

  def startNetwork[F[_]: Async: Logger](
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
    networkAlgebra:              NetworkAlgebra[F],
    initialHosts:                List[RemoteAddress],
    networkProperties:           NetworkProperties,
    clock:                       ClockAlgebra[F],
    addRemotePeerAlgebra:        PeerCreationRequestAlgebra[F],
    closedPeers:                 Stream[F, ConnectedPeer],
    hotPeersUpdate:              Set[RemoteAddress] => F[Unit]
  ): Resource[F, PeersManagerActor[F]] =
    for {
      _            <- Resource.liftK(Logger[F].info(s"Start actors network with list of peers: $initialHosts"))
      slotDuration <- Resource.liftK(clock.slotLength)
      p2pNetworkConfig = P2PNetworkConfig(networkProperties, slotDuration)

      peersFromStorage <- Resource.liftK(knownHostsStore.get(()).map(_.getOrElse(Seq.empty).map(_.asRemoteAddress)))
      _                <- Resource.liftK(Logger[F].info(s"Loaded from storage next known hosts: $peersFromStorage"))
      peerManager <- networkAlgebra.makePeerManger(
        thisHostId,
        networkAlgebra,
        localChain,
        slotDataStore,
        transactionStore,
        blockIdTree,
        headerToBodyValidation,
        addRemotePeerAlgebra,
        p2pNetworkConfig,
        hotPeersUpdate,
        buildSaveRemotePeersFunction(knownHostsStore)
      )

      reputationAggregator <- networkAlgebra.makeReputationAggregation(peerManager, p2pNetworkConfig)

      requestsProxy <- networkAlgebra.makeRequestsProxy(reputationAggregator, peerManager, headerStore, bodyStore)
      blocksChecker <- networkAlgebra.makeBlockChecker(
        reputationAggregator,
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
      _ <- Resource.liftK(peerManager.sendNoWait(PeersManager.Message.SetupReputationAggregator(reputationAggregator)))
      _ <- Resource.liftK(peerManager.sendNoWait(PeersManager.Message.SetupBlockChecker(blocksChecker)))
      _ <- Resource.liftK(peerManager.sendNoWait(PeersManager.Message.SetupRequestsProxy(requestsProxy)))

      _ <- Resource.liftK(
        NonEmptyChain
          .fromSeq(initialHosts ++ peersFromStorage)
          .map { initialPeers =>
            peerManager.sendNoWait(PeersManager.Message.AddKnownPeers(initialPeers))
          }
          .getOrElse(Logger[F].error(show"No know hosts are set during node startup"))
      )

      notifier <- networkAlgebra.makeNotifier(peerManager, reputationAggregator, p2pNetworkConfig)
      _        <- Resource.liftK(notifier.sendNoWait(Notifier.Message.StartNotifications))

      _ <- startDisconnectedPeersNotifier(peerManager, closedPeers)

    } yield peerManager

  private def startDisconnectedPeersNotifier[F[_]: Async: Logger](
    peersManager: PeersManagerActor[F],
    closedPeers:  Stream[F, ConnectedPeer]
  ): Resource[F, F[Outcome[F, Throwable, Unit]]] =
    closedPeers
      .map(closed =>
        Stream.resource(
          for {
            _ <- Resource.liftK(Logger[F].info(s"Remote peer ${closed.remoteAddress} closing had been detected"))
            _ <- Resource.liftK(peersManager.sendNoWait(PeersManager.Message.ClosePeer(closed.remoteAddress.host)))
          } yield ()
        )
      )
      .parJoinUnbounded
      .compile
      .drain
      .background

  private def buildSaveRemotePeersFunction[F[_]: Async: Logger](
    knownHostsStore: Store[F, Unit, Seq[KnownHost]]
  ): Set[RemoteAddress] => F[Unit] = { peers: Set[RemoteAddress] =>
    Logger[F].info(s"Going to save known hosts $peers to local data storage") >>
    knownHostsStore.put((), peers.map(_.asKnownHost).toList)
  }
}
