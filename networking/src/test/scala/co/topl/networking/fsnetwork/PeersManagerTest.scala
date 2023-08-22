package co.topl.networking.fsnetwork

import cats.Applicative
import cats.data.NonEmptyChain
import cats.effect.{IO, Resource}
import cats.implicits._
import co.topl.algebras.Store
import co.topl.brambl.generators.TransactionGenerator
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.config.ApplicationConfig.Bifrost.NetworkProperties
import co.topl.consensus.algebras.{BlockHeaderToBodyValidationAlgebra, LocalChainAlgebra}
import co.topl.consensus.models.{BlockId, SlotData}
import co.topl.eventtree.ParentChildTree
import co.topl.networking.blockchain.BlockchainPeerClient
import co.topl.networking.fsnetwork.BlockChecker.BlockCheckerActor
import co.topl.networking.fsnetwork.PeerActor.PeerActor
import co.topl.networking.fsnetwork.PeersManager.Peer
import co.topl.networking.fsnetwork.PeersManagerTest.F
import co.topl.networking.fsnetwork.ReputationAggregator.ReputationAggregatorActor
import co.topl.networking.fsnetwork.RequestsProxy.RequestsProxyActor
import co.topl.networking.p2p.RemoteAddress
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration.{FiniteDuration, SECONDS}

object PeersManagerTest {
  type F[A] = IO[A]
}

class PeersManagerTest
    extends CatsEffectSuite
    with ScalaCheckEffectSuite
    with AsyncMockFactory
    with TransactionGenerator {
  implicit val logger: Logger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)

  val thisHostId: HostId = "0.0.0.0"
  val hostId: HostId = "127.0.0.1"

  val defaultColdToWarmSelector: ColdToWarmSelector = (coldHosts: Set[RemoteAddress], countToReceive: Int) =>
    coldHosts.toSeq.sortBy(_.port).take(countToReceive).toSet

  val defaultHotPeerUpdater: Set[RemoteAddress] => F[Unit] = _ => Applicative[F].unit
  val defaultPeersSaver: Set[RemoteAddress] => F[Unit] = _ => Applicative[F].unit

  implicit val dummyDns: DnsResolver[F] = (host: HostId) => Option(host).pure[F]

  test("Banned peer shall be stopped and appropriate state shall be set") {
    withMock {

      val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]
      val localChain: LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]]
      val slotDataStore: Store[F, BlockId, SlotData] = mock[Store[F, BlockId, SlotData]]
      val transactionStore: Store[F, TransactionId, IoTransaction] = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree: ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val p2pConfig: P2PNetworkConfig = P2PNetworkConfig(NetworkProperties(), FiniteDuration(1, SECONDS))

      val peerActor = mock[PeerActor[F]]
      (peerActor.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = false, applicationLevel = false))
        .returns(().pure[F])
      (() => peerActor.id).expects().anyNumberOfTimes().returns(1) // used in release actor function

      val reputationAggregator = mock[ReputationAggregatorActor[F]]
      (reputationAggregator.sendNoWait _)
        .expects(ReputationAggregator.Message.PeerIsCold(hostId))
        .returns(().pure[F])

      val initialPeersMap =
        Map.empty[HostId, Peer[F]] + (hostId -> Peer(PeerState.Hot, Option(peerActor), None))

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          headerToBodyValidation,
          newPeerCreationAlgebra,
          p2pConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          initialPeers = initialPeersMap
        )
        .use { actor =>
          for {
            _        <- actor.send(PeersManager.Message.SetupReputationAggregator(reputationAggregator))
            endState <- actor.send(PeersManager.Message.BanPeer(hostId))
            _ = assert(endState.peers(hostId).state == PeerState.Banned)
          } yield ()
        }
    }
  }

  test("Peer moved to cold state shall be stopped and appropriate state shall be set") {
    withMock {

      val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]
      val localChain: LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]]
      val slotDataStore: Store[F, BlockId, SlotData] = mock[Store[F, BlockId, SlotData]]
      val transactionStore: Store[F, TransactionId, IoTransaction] = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree: ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val p2pConfig: P2PNetworkConfig = P2PNetworkConfig(NetworkProperties(), FiniteDuration(1, SECONDS))

      val peerActor = mock[PeerActor[F]]
      (peerActor.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = false, applicationLevel = false))
        .returns(().pure[F])
      (() => peerActor.id).expects().anyNumberOfTimes().returns(1) // used in release actor function

      val reputationAggregator = mock[ReputationAggregatorActor[F]]
      (reputationAggregator.sendNoWait _)
        .expects(ReputationAggregator.Message.PeerIsCold(hostId))
        .returns(().pure[F])

      val initialPeersMap =
        Map.empty[HostId, Peer[F]] + (hostId -> Peer(PeerState.Hot, Option(peerActor), None))

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          headerToBodyValidation,
          newPeerCreationAlgebra,
          p2pConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          initialPeers = initialPeersMap
        )
        .use { actor =>
          for {
            _        <- actor.send(PeersManager.Message.SetupReputationAggregator(reputationAggregator))
            endState <- actor.send(PeersManager.Message.ClosePeer(hostId))
            _ = assert(endState.peers(hostId).state == PeerState.Cold)
          } yield ()
        }
    }
  }

  test("Reputation update: If no warm peer then move cold peer(s) to prewarm") {
    withMock {
      val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]
      val localChain: LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]]
      val slotDataStore: Store[F, BlockId, SlotData] = mock[Store[F, BlockId, SlotData]]
      val transactionStore: Store[F, TransactionId, IoTransaction] = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree: ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val p2pConfig: P2PNetworkConfig =
        P2PNetworkConfig(NetworkProperties(minimumWarmConnections = 2), FiniteDuration(1, SECONDS))
      val reputationAggregator = mock[ReputationAggregatorActor[F]]

      val host1 = RemoteAddress("first", 0)
      val host2 = RemoteAddress("second", 1)
      val host3 = RemoteAddress("third", 2)

      (newPeerCreationAlgebra.requestNewPeerCreation _).expects(host1).returns(().pure[F])
      (newPeerCreationAlgebra.requestNewPeerCreation _).expects(host2).returns(().pure[F])

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          headerToBodyValidation,
          newPeerCreationAlgebra,
          p2pConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          coldToWarmSelector = defaultColdToWarmSelector
        )
        .use { actor =>
          for {
            _            <- actor.send(PeersManager.Message.SetupReputationAggregator(reputationAggregator))
            withColdPeer <- actor.send(PeersManager.Message.AddKnownPeers(NonEmptyChain(host1, host2, host3)))
            _ = assert(withColdPeer.peers(host1.host).state == PeerState.Cold)
            _ = assert(withColdPeer.peers(host2.host).state == PeerState.Cold)
            _ = assert(withColdPeer.peers(host3.host).state == PeerState.Cold)

            withUpdate <- actor.send(PeersManager.Message.UpdatedReputation(Map.empty, Map.empty, Map.empty))
            _ = assert(withUpdate.peers(host1.host).state == PeerState.PreWarm)
            _ = assert(withUpdate.peers(host2.host).state == PeerState.PreWarm)
            _ = assert(withUpdate.peers(host3.host).state == PeerState.Cold)
          } yield ()
        }
    }
  }

  test("Adding cold peer: ignore banned peers") {
    withMock {
      val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]
      val localChain: LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]]
      val slotDataStore: Store[F, BlockId, SlotData] = mock[Store[F, BlockId, SlotData]]
      val transactionStore: Store[F, TransactionId, IoTransaction] = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree: ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val p2pConfig: P2PNetworkConfig =
        P2PNetworkConfig(NetworkProperties(minimumWarmConnections = 2), FiniteDuration(1, SECONDS))
      val reputationAggregator = mock[ReputationAggregatorActor[F]]

      val host1 = RemoteAddress("1", 1)
      val host2 = RemoteAddress("2", 2)
      val host3 = RemoteAddress("3", 3)
      val host4 = RemoteAddress("4", 4)

      val initialPeersMap: Map[HostId, Peer[F]] =
        Map(
          host1.host -> Peer(PeerState.Hot, None, Option(1)),
          host2.host -> Peer(PeerState.Banned, None, None),
          host3.host -> Peer(PeerState.Cold, None, Option(3))
        )

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          headerToBodyValidation,
          newPeerCreationAlgebra,
          p2pConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          initialPeersMap
        )
        .use { actor =>
          for {
            _            <- actor.send(PeersManager.Message.SetupReputationAggregator(reputationAggregator))
            withColdPeer <- actor.send(PeersManager.Message.AddKnownPeers(NonEmptyChain(host1, host2, host3, host4)))
            _ = assert(withColdPeer.peers(host1.host).state == PeerState.Hot)
            _ = assert(withColdPeer.peers(host2.host).state == PeerState.Banned)
            _ = assert(withColdPeer.peers(host3.host).state == PeerState.Cold)
            _ = assert(withColdPeer.peers(host4.host).state == PeerState.Cold)

          } yield ()
        }
    }
  }

  test("Reputation update: close opened hot connections") {
    withMock {
      val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]
      val localChain: LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]]
      val slotDataStore: Store[F, BlockId, SlotData] = mock[Store[F, BlockId, SlotData]]
      val transactionStore: Store[F, TransactionId, IoTransaction] = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree: ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val p2pConfig: P2PNetworkConfig = P2PNetworkConfig(NetworkProperties(), FiniteDuration(1, SECONDS))
      val reputationAggregator = mock[ReputationAggregatorActor[F]]

      val host1 = RemoteAddress("first", 1)
      val peer1 = mock[PeerActor[F]]

      val host2 = RemoteAddress("second", 2)
      val peer2 = mock[PeerActor[F]]

      val host3 = RemoteAddress("third", 3)
      val peer3 = mock[PeerActor[F]]

      val host4 = RemoteAddress("fourth", 4)
      val peer4 = mock[PeerActor[F]]

      val host5 = RemoteAddress("five", 5)
      val peer5 = mock[PeerActor[F]]
      (peer5.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = false, applicationLevel = false))
        .returns(().pure[F])
      (() => peer5.id).expects().anyNumberOfTimes().returns(5) // used in release actor function
      (reputationAggregator.sendNoWait _)
        .expects(ReputationAggregator.Message.PeerIsCold(host5.host))
        .returns(().pure[F])

      val host6 = RemoteAddress("six", 6)
      val peer6 = mock[PeerActor[F]]
      (peer6.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = false, applicationLevel = false))
        .returns(().pure[F])
      (() => peer6.id).expects().anyNumberOfTimes().returns(6) // used in release actor function
      (reputationAggregator.sendNoWait _)
        .expects(ReputationAggregator.Message.PeerIsCold(host6.host))
        .returns(().pure[F])

      val initialPeersMap: Map[HostId, Peer[F]] =
        Map(
          host1.host -> Peer(PeerState.Hot, Option(peer1), Option(1)),
          host2.host -> Peer(PeerState.Hot, Option(peer2), None),
          host3.host -> Peer(PeerState.Hot, Option(peer3), Option(3)),
          host4.host -> Peer(PeerState.Hot, Option(peer4), None),
          host5.host -> Peer(PeerState.Hot, Option(peer5), None),
          host6.host -> Peer(PeerState.Hot, Option(peer6), None)
        )

      val performanceRep: Map[HostId, HostReputationValue] =
        Map(
          host1.host -> 1.0,
          host3.host -> p2pConfig.networkProperties.minimumRequiredReputation * 1.05
        )

      val blockRep: Map[HostId, HostReputationValue] =
        Map(
          host2.host -> 1.0,
          host3.host -> p2pConfig.networkProperties.minimumRequiredReputation * 1.05
        )

      val noveltyRep: Map[HostId, Long] =
        Map(
          host4.host -> 1
        )

      val hotUpdater = mock[Set[RemoteAddress] => F[Unit]]
      (hotUpdater.apply _).expects(Set(host1, host3)).once().returns(().pure[F])

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          headerToBodyValidation,
          newPeerCreationAlgebra,
          p2pConfig,
          hotUpdater,
          defaultPeersSaver,
          coldToWarmSelector = defaultColdToWarmSelector,
          initialPeers = initialPeersMap
        )
        .use { actor =>
          for {
            _          <- actor.send(PeersManager.Message.SetupReputationAggregator(reputationAggregator))
            withUpdate <- actor.send(PeersManager.Message.UpdatedReputation(performanceRep, blockRep, noveltyRep))
            _ = assert(withUpdate.peers(host1.host).state == PeerState.Hot)
            _ = assert(withUpdate.peers(host2.host).state == PeerState.Hot)
            _ = assert(withUpdate.peers(host3.host).state == PeerState.Hot)
            _ = assert(withUpdate.peers(host4.host).state == PeerState.Hot)
            _ = assert(withUpdate.peers(host5.host).state == PeerState.Cold)
            _ = assert(withUpdate.peers(host6.host).state == PeerState.Cold)
          } yield ()
        }
    }
  }

  test("Reputation update: move warm to hot") {
    withMock {
      val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]
      val localChain: LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]]
      val slotDataStore: Store[F, BlockId, SlotData] = mock[Store[F, BlockId, SlotData]]
      val transactionStore: Store[F, TransactionId, IoTransaction] = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree: ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val p2pConfig: P2PNetworkConfig =
        P2PNetworkConfig(NetworkProperties(minimumHotConnections = 2), FiniteDuration(1, SECONDS))
      val reputationAggregator = mock[ReputationAggregatorActor[F]]

      val host1 = RemoteAddress("first", 1)
      val peer1 = mock[PeerActor[F]]
      (peer1.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = true, applicationLevel = true))
        .returns(().pure[F])

      val host2 = RemoteAddress("second", 2)
      val peer2 = mock[PeerActor[F]]
      (peer2.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = true, applicationLevel = true))
        .returns(().pure[F])

      val host3 = RemoteAddress("third", 3)
      val peer3 = mock[PeerActor[F]]

      val host4 = RemoteAddress("fourth", 4)
      val peer4 = mock[PeerActor[F]]

      val initialPeersMap: Map[HostId, Peer[F]] =
        Map(
          host1.host -> Peer(PeerState.Warm, Option(peer1), Option(1)),
          host2.host -> Peer(PeerState.Warm, Option(peer2), Option(2)),
          host3.host -> Peer(PeerState.Warm, Option(peer3), None),
          host4.host -> Peer(PeerState.Warm, Option(peer4), None)
        )

      (reputationAggregator.sendNoWait _)
        .expects(ReputationAggregator.Message.NewHotPeer(NonEmptyChain(host2.host, host1.host)))
        .once()
        .returns(().pure[F])

      val performanceRep: Map[HostId, HostReputationValue] =
        Map(
          host1.host -> 1.0,
          host2.host -> 0.9,
          host3.host -> 0.8,
          host4.host -> 0.7
        )

      val hotUpdater = mock[Set[RemoteAddress] => F[Unit]]
      (hotUpdater.apply _).expects(Set(host1, host2)).once().returns(().pure[F])

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          headerToBodyValidation,
          newPeerCreationAlgebra,
          p2pConfig,
          hotUpdater,
          defaultPeersSaver,
          coldToWarmSelector = defaultColdToWarmSelector,
          initialPeers = initialPeersMap
        )
        .use { actor =>
          for {
            _          <- actor.send(PeersManager.Message.SetupReputationAggregator(reputationAggregator))
            withUpdate <- actor.send(PeersManager.Message.UpdatedReputation(performanceRep, Map.empty, Map.empty))
            _ = assert(withUpdate.peers(host1.host).state == PeerState.Hot)
            _ = assert(withUpdate.peers(host2.host).state == PeerState.Hot)
            _ = assert(withUpdate.peers(host3.host).state == PeerState.Warm)
            _ = assert(withUpdate.peers(host4.host).state == PeerState.Warm)
          } yield ()
        }
    }
  }

  test("Peer moved to warm state after received new opened peer message") {
    withMock {

      val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]
      val localChain: LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]]
      val slotDataStore: Store[F, BlockId, SlotData] = mock[Store[F, BlockId, SlotData]]
      val transactionStore: Store[F, TransactionId, IoTransaction] = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree: ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val p2pConfig: P2PNetworkConfig = P2PNetworkConfig(NetworkProperties(), FiniteDuration(1, SECONDS))
      val blockChecker = mock[BlockCheckerActor[F]]
      val reputationAggregator = mock[ReputationAggregatorActor[F]]
      val requestProxy = mock[RequestsProxyActor[F]]

      val host1 = RemoteAddress("first", 1)
      val peer1 = mock[PeerActor[F]]
      (networkAlgebra.makePeer _).expects(host1.host, *, *, *, *, *, *, *, *, *).once().returns(Resource.pure(peer1))
      (() => peer1.id).expects().anyNumberOfTimes().returns(1)
      (peer1.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = true, applicationLevel = false))
        .returns(Applicative[F].unit)
      (peer1.sendNoWait _)
        .expects(PeerActor.Message.GetNetworkQuality)
        .returns(Applicative[F].unit)
      (peer1.sendNoWait _)
        .expects(PeerActor.Message.GetPeerServerAddress)
        .returns(Applicative[F].unit)

      val host2 = RemoteAddress("second", 2)
      val peer2 = mock[PeerActor[F]]
      (networkAlgebra.makePeer _).expects(host2.host, *, *, *, *, *, *, *, *, *).once().returns(Resource.pure(peer2))
      (() => peer2.id).expects().anyNumberOfTimes().returns(2)
      (peer2.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = true, applicationLevel = false))
        .returns(Applicative[F].unit)
      (peer2.sendNoWait _)
        .expects(PeerActor.Message.GetNetworkQuality)
        .returns(Applicative[F].unit)
      (peer2.sendNoWait _)
        .expects(PeerActor.Message.GetPeerServerAddress)
        .returns(Applicative[F].unit)

      val host3 = RemoteAddress("3", 3)
      val peer3 = mock[PeerActor[F]]
      (networkAlgebra.makePeer _).expects(host3.host, *, *, *, *, *, *, *, *, *).once().returns(Resource.pure(peer3))
      (() => peer3.id).expects().anyNumberOfTimes().returns(2)
      (peer3.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = true, applicationLevel = false))
        .returns(Applicative[F].unit)
      (peer3.sendNoWait _)
        .expects(PeerActor.Message.GetNetworkQuality)
        .returns(Applicative[F].unit)
      (peer3.sendNoWait _)
        .expects(PeerActor.Message.GetPeerServerAddress)
        .returns(Applicative[F].unit)

      val host4 = RemoteAddress("4", 4)
      val host5 = RemoteAddress("5", 5)

      val initialPeersMap = Map(
        host1.host -> Peer(PeerState.PreWarm, None, None),
        host3.host -> Peer(PeerState.PreWarm, None, None),
        host4.host -> Peer(PeerState.Banned, None, None),
        host5.host -> Peer(PeerState.Hot, None, None)
      )

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          headerToBodyValidation,
          newPeerCreationAlgebra,
          p2pConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          initialPeers = initialPeersMap
        )
        .use { actor =>
          for {
            _          <- actor.send(PeersManager.Message.SetupReputationAggregator(reputationAggregator))
            _          <- actor.send(PeersManager.Message.SetupBlockChecker(blockChecker))
            _          <- actor.send(PeersManager.Message.SetupRequestsProxy(requestProxy))
            stateHost1 <- actor.send(PeersManager.Message.OpenedPeerConnection(host1, mock[BlockchainPeerClient[F]]))
            _ = assert(stateHost1.peers(host1.host).state == PeerState.Warm)
            stateHost2 <- actor.send(PeersManager.Message.OpenedPeerConnection(host2, mock[BlockchainPeerClient[F]]))
            _ = assert(stateHost2.peers(host2.host).state == PeerState.Warm)
            stateHost3 <- actor.send(PeersManager.Message.OpenedPeerConnection(host3, mock[BlockchainPeerClient[F]]))
            _ = assert(stateHost3.peers(host3.host).state == PeerState.Warm)
            stateHost4 <- actor.send(PeersManager.Message.OpenedPeerConnection(host4, mock[BlockchainPeerClient[F]]))
            _ = assert(stateHost4.peers(host4.host).state == PeerState.Banned)
            stateHost5 <- actor.send(PeersManager.Message.OpenedPeerConnection(host5, mock[BlockchainPeerClient[F]]))
            _ = assert(stateHost5.peers(host5.host).state == PeerState.Hot)
          } yield ()
        }
    }
  }

  test("Adding prewarm hosts shall initiate peer connection for cold peers") {
    withMock {
      val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]
      val localChain: LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]]
      val slotDataStore: Store[F, BlockId, SlotData] = mock[Store[F, BlockId, SlotData]]
      val transactionStore: Store[F, TransactionId, IoTransaction] = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree: ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val p2pConfig: P2PNetworkConfig = P2PNetworkConfig(NetworkProperties(), FiniteDuration(1, SECONDS))
      val blockChecker = mock[BlockCheckerActor[F]]
      val reputationAggregator = mock[ReputationAggregatorActor[F]]
      val requestProxy = mock[RequestsProxyActor[F]]

      val host1 = RemoteAddress("1", 1)
      val host2 = RemoteAddress("2", 2)
      val host3 = RemoteAddress("3", 3)
      val host4 = RemoteAddress("4", 4)
      val host5 = RemoteAddress("5", 5)

      (newPeerCreationAlgebra.requestNewPeerCreation _).expects(host1).once().returns(Applicative[F].unit)
      (newPeerCreationAlgebra.requestNewPeerCreation _).expects(host2).once().returns(Applicative[F].unit)

      val initialPeersMap: Map[HostId, Peer[F]] = Map(
        host1.host -> Peer(PeerState.Cold, None, None),
        host3.host -> Peer(PeerState.Banned, None, None),
        host4.host -> Peer(PeerState.Warm, None, None),
        host5.host -> Peer(PeerState.Hot, None, None)
      )

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          headerToBodyValidation,
          newPeerCreationAlgebra,
          p2pConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          initialPeers = initialPeersMap
        )
        .use { actor =>
          for {
            _ <- actor.send(PeersManager.Message.SetupReputationAggregator(reputationAggregator))
            _ <- actor.send(PeersManager.Message.SetupBlockChecker(blockChecker))
            _ <- actor.send(PeersManager.Message.SetupRequestsProxy(requestProxy))
            toSend = NonEmptyChain(host1, host2, host3, host4, host5)
            newState <- actor.send(PeersManager.Message.AddPreWarmPeers(toSend))
            _ = assert(newState.peers(host1.host).state == PeerState.PreWarm)
            _ = assert(newState.peers(host2.host).state == PeerState.PreWarm)
            _ = assert(newState.peers(host3.host).state == PeerState.Banned)
            _ = assert(newState.peers(host4.host).state == PeerState.Warm)
            _ = assert(newState.peers(host5.host).state == PeerState.Hot)
          } yield ()
        }
    }
  }

  test("Receiving remote peer address shall update it") {
    withMock {
      val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]
      val localChain: LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]]
      val slotDataStore: Store[F, BlockId, SlotData] = mock[Store[F, BlockId, SlotData]]
      val transactionStore: Store[F, TransactionId, IoTransaction] = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree: ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val p2pConfig: P2PNetworkConfig = P2PNetworkConfig(NetworkProperties(), FiniteDuration(1, SECONDS))
      val blockChecker = mock[BlockCheckerActor[F]]
      val reputationAggregator = mock[ReputationAggregatorActor[F]]
      val requestProxy = mock[RequestsProxyActor[F]]

      val host1 = RemoteAddress("first", 1)
      val peer1 = mock[PeerActor[F]]
      val hostServer1Port = 999
      val host1serverAddress = RemoteAddress(host1.host, hostServer1Port)

      val host2 = RemoteAddress("second", 2)
      mock[PeerActor[F]]
      val hostServer2 = RemoteAddress("secondServer", 1)

      val host3 = RemoteAddress("third", 3)
      val peer3 = mock[PeerActor[F]]

      val host4 = RemoteAddress("fourth", 4)
      val peer4 = mock[PeerActor[F]]
      val hostServer4Port = 999
      val host4serverAddress = RemoteAddress(host4.host, hostServer4Port)

      val host5 = RemoteAddress("five", 5)
      val peer5 = mock[PeerActor[F]]
      val hostServer5 = RemoteAddress("fiveServer", 5)

      val initialPeersMap: Map[HostId, Peer[F]] =
        Map(
          host1.host -> Peer(PeerState.Hot, Option(peer1), None),
          host3.host -> Peer(PeerState.Hot, Option(peer3), None),
          host4.host -> Peer(PeerState.Hot, Option(peer4), Option(host4serverAddress.port)),
          host5.host -> Peer(PeerState.Warm, Option(peer5), Option(hostServer5.port))
        )

      val hotUpdater = mock[Set[RemoteAddress] => F[Unit]]
      (hotUpdater.apply _).expects(Set(host1serverAddress, host4serverAddress)).twice().returns(().pure[F])

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          headerToBodyValidation,
          newPeerCreationAlgebra,
          p2pConfig,
          hotUpdater,
          defaultPeersSaver,
          initialPeers = initialPeersMap
        )
        .use { actor =>
          for {
            _         <- actor.send(PeersManager.Message.SetupReputationAggregator(reputationAggregator))
            _         <- actor.send(PeersManager.Message.SetupBlockChecker(blockChecker))
            _         <- actor.send(PeersManager.Message.SetupRequestsProxy(requestProxy))
            newState1 <- actor.send(PeersManager.Message.RemotePeerServerPort(host1.host, hostServer1Port))
            _ = assert(newState1.peers(host1.host).serverPort == Option(host1serverAddress.port))
            newState2 <- actor.send(PeersManager.Message.RemotePeerServerPort(host2.host, hostServer2.port))
            _ = assert(newState1.peers == newState2.peers)
          } yield ()
        }
    }
  }

  test("Finalized actor shall write non-banned hosts") {
    withMock {
      val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]
      val localChain: LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]]
      val slotDataStore: Store[F, BlockId, SlotData] = mock[Store[F, BlockId, SlotData]]
      val transactionStore: Store[F, TransactionId, IoTransaction] = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree: ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val p2pConfig: P2PNetworkConfig = P2PNetworkConfig(NetworkProperties(), FiniteDuration(1, SECONDS))
      val blockChecker = mock[BlockCheckerActor[F]]
      val reputationAggregator = mock[ReputationAggregatorActor[F]]
      val requestProxy = mock[RequestsProxyActor[F]]

      val host1 = "1"
      val hostServer1Port = 1

      val host2 = "2"
      val hostServer2Port = 2

      val host3 = "3"
      val hostServer3Port = 3

      val host4 = "4"
      val hostServer4Port = 4

      val host5 = "5"
      val hostServer5Port = 5

      val host6 = "6"

      val initialPeersMap: Map[HostId, Peer[F]] =
        Map(
          host1 -> Peer(PeerState.Banned, None, Option(hostServer1Port)),
          host2 -> Peer(PeerState.Cold, None, Option(hostServer2Port)),
          host3 -> Peer(PeerState.PreWarm, None, Option(hostServer3Port)),
          host4 -> Peer(PeerState.Warm, None, Option(hostServer4Port)),
          host5 -> Peer(PeerState.Hot, None, Option(hostServer5Port)),
          host6 -> Peer(PeerState.Hot, None, None)
        )

      val writingHosts = mock[Set[RemoteAddress] => F[Unit]]
      val expectedHosts = Set(
        RemoteAddress(host2, hostServer2Port),
        RemoteAddress(host3, hostServer3Port),
        RemoteAddress(host4, hostServer4Port),
        RemoteAddress(host5, hostServer5Port)
      )
      (writingHosts.apply _).expects(expectedHosts).once().returns(().pure[F])

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          headerToBodyValidation,
          newPeerCreationAlgebra,
          p2pConfig,
          defaultHotPeerUpdater,
          writingHosts,
          initialPeers = initialPeersMap
        )
        .use { actor =>
          for {
            _ <- actor.send(PeersManager.Message.SetupReputationAggregator(reputationAggregator))
            _ <- actor.send(PeersManager.Message.SetupBlockChecker(blockChecker))
            _ <- actor.send(PeersManager.Message.SetupRequestsProxy(requestProxy))
          } yield ()
        }
    }
  }
}
