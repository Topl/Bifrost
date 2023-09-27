package co.topl.networking.fsnetwork

import cats.Applicative
import cats.data.NonEmptyChain
import cats.effect.{IO, Resource}
import cats.implicits._
import co.topl.algebras.Store
import co.topl.brambl.generators.TransactionGenerator
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import co.topl.config.ApplicationConfig.Bifrost.NetworkProperties
import co.topl.consensus.algebras.{BlockHeaderToBodyValidationAlgebra, LocalChainAlgebra}
import co.topl.consensus.models.{BlockId, SlotData}
import co.topl.eventtree.ParentChildTree
import co.topl.models.ModelGenerators.GenHelper
import co.topl.models.generators.consensus.ModelGenerators
import co.topl.models.generators.consensus.ModelGenerators.nonEmptyChainArbOfLen
import co.topl.networking.blockchain.BlockchainPeerClient
import co.topl.networking.fsnetwork.BlockChecker.BlockCheckerActor
import co.topl.networking.fsnetwork.PeerActor.PeerActor
import co.topl.networking.fsnetwork.PeersManagerTest.F
import co.topl.networking.fsnetwork.ReputationAggregator.ReputationAggregatorActor
import co.topl.networking.fsnetwork.RequestsProxy.RequestsProxyActor
import co.topl.networking.fsnetwork.TestHelper.{arbitraryHost, arbitraryHostBlockId}
import co.topl.networking.p2p.RemoteAddress
import com.github.benmanes.caffeine.cache.Caffeine
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory
import org.scalamock.util.Defaultable.defaultInt
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration.{FiniteDuration, SECONDS}
import scala.jdk.CollectionConverters._

object PeersManagerTest {
  type F[A] = IO[A]
}

class PeersManagerTest
    extends CatsEffectSuite
    with ScalaCheckEffectSuite
    with AsyncMockFactory
    with TransactionGenerator {
  implicit val logger: Logger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)
  val maxChainSize = 99

  val thisHostId: HostId = "0.0.0.0"
  val hostId: HostId = "127.0.0.1"

  val defaultColdToWarmSelector: ColdToWarmSelector[F] =
    (coldHosts: Set[PeerWithHostAndPort[F]], countToReceive: Int) =>
      coldHosts.toSeq.sortBy(_.serverPort).take(countToReceive).map(_.asRemoteAddress).toSet

  val defaultHotPeerUpdater: Set[RemoteAddress] => F[Unit] = _ => Applicative[F].unit
  val defaultPeersSaver: Set[RemoteAddress] => F[Unit] = _ => Applicative[F].unit

  val defaultP2PConfig: P2PNetworkConfig =
    P2PNetworkConfig(NetworkProperties(closeTimeoutWindowInMs = Long.MaxValue), FiniteDuration(1, SECONDS))

  implicit val dummyDns: DnsResolver[F] = (host: HostId) => Option(host).pure[F]

  def mockPeerActor[F[_]: Applicative](): PeerActor[F] = {
    val mocked = mock[PeerActor[F]]
    (() => mocked.id).stubs().returns(mocked.hashCode())
    (mocked.mailboxSize _).stubs().returns(0.pure[F])

    mocked
  }

  test("Get current tips request shall be forwarded if application level is enabled") {
    withMock {

      val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]
      val localChain: LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]]
      val slotDataStore: Store[F, BlockId, SlotData] = mock[Store[F, BlockId, SlotData]]
      val transactionStore: Store[F, TransactionId, IoTransaction] = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree: ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val reputationAggregator = mock[ReputationAggregatorActor[F]]

      val coldHost = arbitraryHost.arbitrary.first
      val coldPeer = mockPeerActor[F]()

      val warmHost = arbitraryHost.arbitrary.first
      val warmPeer = mockPeerActor[F]()

      val hotHost = arbitraryHost.arbitrary.first
      val hotPeer = mockPeerActor[F]()
      (hotPeer.sendNoWait _)
        .expects(PeerActor.Message.GetCurrentTip)
        .returns(().pure[F])

      val banedHost = arbitraryHost.arbitrary.first
      val banedPeer = mockPeerActor[F]()

      val initialPeersMap =
        Map[HostId, Peer[F]](
          coldHost  -> Peer(PeerState.Cold, Option(coldPeer), None, Seq.empty, remoteNetworkLevel = true),
          warmHost  -> Peer(PeerState.Warm, Option(warmPeer), None, Seq.empty, remoteNetworkLevel = true),
          hotHost   -> Peer(PeerState.Hot, Option(hotPeer), None, Seq.empty, remoteNetworkLevel = true),
          banedHost -> Peer(PeerState.Banned, Option(banedPeer), None, Seq.empty, remoteNetworkLevel = true)
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
          defaultP2PConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          initialPeersMap,
          Caffeine.newBuilder.maximumSize(blockSourceCacheSize).build[BlockId, Set[HostId]]()
        )
        .use { actor =>
          for {
            _ <- actor.send(PeersManager.Message.SetupReputationAggregator(reputationAggregator))
            _ <- actor.send(PeersManager.Message.GetCurrentTips)
          } yield ()
        }
    }
  }

  test("Get network quality shall be forwarded to warm hosts") {
    withMock {

      val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]
      val localChain: LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]]
      val slotDataStore: Store[F, BlockId, SlotData] = mock[Store[F, BlockId, SlotData]]
      val transactionStore: Store[F, TransactionId, IoTransaction] = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree: ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val reputationAggregator = mock[ReputationAggregatorActor[F]]

      val coldHost = arbitraryHost.arbitrary.first
      val coldPeer = mockPeerActor[F]()

      val warmHost = arbitraryHost.arbitrary.first
      val warmPeer = mockPeerActor[F]()
      (warmPeer.sendNoWait _)
        .expects(PeerActor.Message.GetNetworkQuality)
        .returns(().pure[F])

      val hotHost = arbitraryHost.arbitrary.first
      val hotPeer = mockPeerActor[F]()

      val banedHost = arbitraryHost.arbitrary.first
      val banedPeer = mockPeerActor[F]()

      val initialPeersMap =
        Map[HostId, Peer[F]](
          coldHost  -> Peer(PeerState.Cold, Option(coldPeer), None, Seq.empty, remoteNetworkLevel = true),
          warmHost  -> Peer(PeerState.Warm, Option(warmPeer), None, Seq.empty, remoteNetworkLevel = true),
          hotHost   -> Peer(PeerState.Hot, Option(hotPeer), None, Seq.empty, remoteNetworkLevel = true),
          banedHost -> Peer(PeerState.Banned, Option(banedPeer), None, Seq.empty, remoteNetworkLevel = true)
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
          defaultP2PConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          initialPeersMap,
          Caffeine.newBuilder.maximumSize(blockSourceCacheSize).build[BlockId, Set[HostId]]()
        )
        .use { actor =>
          for {
            _ <- actor.send(PeersManager.Message.SetupReputationAggregator(reputationAggregator))
            _ <- actor.send(PeersManager.Message.GetNetworkQualityForWarmHosts)
          } yield ()
        }
    }
  }

  test("Add local address shall update state") {
    withMock {

      val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]
      val localChain: LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]]
      val slotDataStore: Store[F, BlockId, SlotData] = mock[Store[F, BlockId, SlotData]]
      val transactionStore: Store[F, TransactionId, IoTransaction] = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree: ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val reputationAggregator = mock[ReputationAggregatorActor[F]]

      val addedAddress1 = RemoteAddress("host", 9090)
      val addedAddress2 = RemoteAddress("host2", 1010)

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
          defaultP2PConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          Map.empty,
          Caffeine.newBuilder.maximumSize(blockSourceCacheSize).build[BlockId, Set[HostId]]()
        )
        .use { actor =>
          for {
            _             <- actor.send(PeersManager.Message.SetupReputationAggregator(reputationAggregator))
            updatedState1 <- actor.send(PeersManager.Message.UpdateThisPeerAddress(addedAddress1))
            _ = assert(updatedState1.thisHostIds.contains(addedAddress1.host))
            updatedState2 <- actor.send(PeersManager.Message.UpdateThisPeerAddress(addedAddress2))
            _ = assert(updatedState2.thisHostIds.contains(addedAddress2.host))
          } yield ()
        }
    }
  }

  test("Banned peer shall be stopped and appropriate state shall be set") {
    withMock {

      val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]
      val localChain: LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]]
      val slotDataStore: Store[F, BlockId, SlotData] = mock[Store[F, BlockId, SlotData]]
      val transactionStore: Store[F, TransactionId, IoTransaction] = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree: ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]

      val peerActor = mockPeerActor[F]()
      (peerActor.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = false, applicationLevel = false))
        .returns(().pure[F])
      (peerActor.sendNoWait _)
        .expects(PeerActor.Message.CloseConnection)
        .returns(().pure[F])

      val reputationAggregator = mock[ReputationAggregatorActor[F]]
      (reputationAggregator.sendNoWait _)
        .expects(ReputationAggregator.Message.StopReputationTracking(Set(hostId)))
        .returns(().pure[F])

      val initialPeersMap =
        Map.empty[HostId, Peer[F]] + (hostId -> Peer(
          PeerState.Hot,
          Option(peerActor),
          None,
          Seq.empty,
          remoteNetworkLevel = true
        ))

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
          defaultP2PConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          initialPeersMap,
          Caffeine.newBuilder.maximumSize(blockSourceCacheSize).build[BlockId, Set[HostId]]()
        )
        .use { actor =>
          for {
            _            <- actor.send(PeersManager.Message.SetupReputationAggregator(reputationAggregator))
            preTimestamp <- System.currentTimeMillis().pure[F]
            endState     <- actor.send(PeersManager.Message.BanPeer(hostId))
            _ = assert(endState.peers(hostId).state == PeerState.Banned)
            _ = assert(endState.peers(hostId).closedTimestamps.size == 1)
            timestamp = endState.peers(hostId).closedTimestamps.head
            _ = assert(timestamp >= preTimestamp)
            _ = assert(timestamp <= System.currentTimeMillis())
          } yield ()
        }
    }
  }

  test("Peer moved to closed state shall be stopped and appropriate state shall be set") {
    withMock {
      val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]
      val localChain: LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]]
      val slotDataStore: Store[F, BlockId, SlotData] = mock[Store[F, BlockId, SlotData]]
      val transactionStore: Store[F, TransactionId, IoTransaction] = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree: ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]

      val peerActor = mockPeerActor[F]()
      (peerActor.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = false, applicationLevel = false))
        .returns(().pure[F])
      (peerActor.sendNoWait _)
        .expects(PeerActor.Message.CloseConnection)
        .returns(().pure[F])

      val reputationAggregator = mock[ReputationAggregatorActor[F]]
      (reputationAggregator.sendNoWait _)
        .expects(ReputationAggregator.Message.StopReputationTracking(Set(hostId)))
        .returns(().pure[F])

      val initialPeersMap =
        Map.empty[HostId, Peer[F]] + (hostId -> Peer(
          PeerState.Hot,
          Option(peerActor),
          None,
          Seq(System.currentTimeMillis()),
          remoteNetworkLevel = true
        ))

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
          defaultP2PConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          initialPeersMap,
          Caffeine.newBuilder.maximumSize(blockSourceCacheSize).build[BlockId, Set[HostId]]()
        )
        .use { actor =>
          for {
            _            <- actor.send(PeersManager.Message.SetupReputationAggregator(reputationAggregator))
            preTimestamp <- System.currentTimeMillis().pure[F]
            endState     <- actor.send(PeersManager.Message.ClosePeer(hostId))
            _ = assert(endState.peers(hostId).actorOpt.isDefined)
            _ = assert(endState.peers(hostId).state == PeerState.Cold)
            _ = assert(endState.peers(hostId).closedTimestamps.size == 2)
            timestamp = endState.peers(hostId).closedTimestamps.last
            _ = assert(timestamp >= preTimestamp)
            _ = assert(timestamp <= System.currentTimeMillis())
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

      val host1 = arbitraryHost.arbitrary.first
      val peerActor1 = mockPeerActor[F]()
      (peerActor1.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = false, applicationLevel = false))
        .returns(().pure[F])
      (peerActor1.sendNoWait _)
        .expects(PeerActor.Message.CloseConnection)
        .returns(().pure[F])

      val host2 = arbitraryHost.arbitrary.first
      val peerActor2 = mockPeerActor[F]()
      (peerActor2.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = false, applicationLevel = false))
        .returns(().pure[F])

      val reputationAggregator = mock[ReputationAggregatorActor[F]]
      (reputationAggregator.sendNoWait _)
        .expects(ReputationAggregator.Message.StopReputationTracking(Set(host1, host2)))
        .returns(().pure[F])

      val initialPeersMap =
        Map(
          host1 -> Peer(
            PeerState.Hot,
            Option(peerActor1),
            None,
            Seq(System.currentTimeMillis()),
            remoteNetworkLevel = false
          ),
          host2 -> Peer(
            PeerState.Warm,
            Option(peerActor2),
            None,
            Seq(System.currentTimeMillis()),
            remoteNetworkLevel = true
          )
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
          defaultP2PConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          initialPeersMap,
          Caffeine.newBuilder.maximumSize(blockSourceCacheSize).build[BlockId, Set[HostId]]()
        )
        .use { actor =>
          for {
            _            <- actor.send(PeersManager.Message.SetupReputationAggregator(reputationAggregator))
            preTimestamp <- System.currentTimeMillis().pure[F]
            endState     <- actor.send(PeersManager.Message.MoveToCold(NonEmptyChain(host1, host2)))
            endTimestamp = System.currentTimeMillis()
            _ = assert(endState.peers(host1).actorOpt.isDefined) // actor will be released on close connection message
            _ = assert(endState.peers(host1).state == PeerState.Cold)
            _ = assert(endState.peers(host1).closedTimestamps.size == 2)
            timestamp1 = endState.peers(host1).closedTimestamps.last
            _ = assert(timestamp1 >= preTimestamp)
            _ = assert(timestamp1 <= endTimestamp)

            _ = assert(endState.peers(host2).actorOpt.isDefined)
            _ = assert(endState.peers(host2).state == PeerState.Cold)
            _ = assert(endState.peers(host2).closedTimestamps.size == 2)
            timestamp2 = endState.peers(host2).closedTimestamps.last
            _ = assert(timestamp2 >= preTimestamp)
            _ = assert(timestamp2 <= endTimestamp)
          } yield ()
        }
    }
  }

  test("Peer moved to cold state shall be stopped closed timestamp shall be updated correctly") {
    withMock {

      val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]
      val localChain: LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]]
      val slotDataStore: Store[F, BlockId, SlotData] = mock[Store[F, BlockId, SlotData]]
      val transactionStore: Store[F, TransactionId, IoTransaction] = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree: ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]

      val peerActor = mockPeerActor[F]()
      (peerActor.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = false, applicationLevel = false))
        .returns(().pure[F])

      val reputationAggregator = mock[ReputationAggregatorActor[F]]
      (reputationAggregator.sendNoWait _)
        .expects(ReputationAggregator.Message.StopReputationTracking(Set(hostId)))
        .returns(().pure[F])

      val timeoutWindows = 1000
      val initialPeersMap =
        Map.empty[HostId, Peer[F]] + (hostId -> Peer(
          PeerState.Hot,
          Option(peerActor),
          None,
          Seq(0, 200, System.currentTimeMillis() - timeoutWindows, System.currentTimeMillis()),
          remoteNetworkLevel = true
        ))

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
          defaultP2PConfig.copy(networkProperties = NetworkProperties(closeTimeoutWindowInMs = timeoutWindows)),
          defaultHotPeerUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          initialPeersMap,
          Caffeine.newBuilder.maximumSize(blockSourceCacheSize).build[BlockId, Set[HostId]]()
        )
        .use { actor =>
          for {
            _            <- actor.send(PeersManager.Message.SetupReputationAggregator(reputationAggregator))
            preTimestamp <- System.currentTimeMillis().pure[F]
            endState     <- actor.send(PeersManager.Message.MoveToCold(NonEmptyChain(hostId)))
            _ = assert(endState.peers(hostId).state == PeerState.Cold)
            _ = assert(endState.peers(hostId).closedTimestamps.size == 2)
            timestamp = endState.peers(hostId).closedTimestamps.last
            _ = assert(timestamp >= preTimestamp)
            _ = assert(timestamp <= System.currentTimeMillis())
          } yield ()
        }
    }
  }

  test("Reputation update: If no warm peer then move cold peer(s) to warm") {
    withMock {
      val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]
      val localChain: LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]]
      val slotDataStore: Store[F, BlockId, SlotData] = mock[Store[F, BlockId, SlotData]]
      val transactionStore: Store[F, TransactionId, IoTransaction] = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree: ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val p2pConfig: P2PNetworkConfig =
        defaultP2PConfig.copy(networkProperties = NetworkProperties(minimumWarmConnections = 2))
      val reputationAggregator = mock[ReputationAggregatorActor[F]]

      val host1 = RemoteAddress("1", 1)
      val host2 = RemoteAddress("2", 2)
      val host3 = RemoteAddress("3", 3)

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
          defaultColdToWarmSelector,
          initialPeers = Map.empty[HostId, Peer[F]],
          blockSource = Caffeine.newBuilder.maximumSize(blockSourceCacheSize).build[BlockId, Set[HostId]]()
        )
        .use { actor =>
          for {
            _            <- actor.send(PeersManager.Message.SetupReputationAggregator(reputationAggregator))
            withColdPeer <- actor.send(PeersManager.Message.AddKnownPeers(NonEmptyChain(host1, host2, host3)))
            _ = assert(withColdPeer.peers(host1.host).state == PeerState.Cold)
            _ = assert(withColdPeer.peers(host2.host).state == PeerState.Cold)
            _ = assert(withColdPeer.peers(host3.host).state == PeerState.Cold)

            withUpdate <- actor.send(PeersManager.Message.UpdatedReputation(Map.empty, Map.empty, Map.empty))
            _ = assert(withUpdate.peers(host1.host).state == PeerState.Warm)
            _ = assert(withUpdate.peers(host2.host).state == PeerState.Warm)
            _ = assert(withUpdate.peers(host3.host).state == PeerState.Cold)
          } yield ()
        }
    }
  }

  test("Reputation update: If no warm peer then move eligible cold peer(s) with port to warm") {
    withMock {
      val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]
      val localChain: LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]]
      val slotDataStore: Store[F, BlockId, SlotData] = mock[Store[F, BlockId, SlotData]]
      val transactionStore: Store[F, TransactionId, IoTransaction] = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree: ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val p2pConfig: P2PNetworkConfig =
        defaultP2PConfig.copy(networkProperties = NetworkProperties(minimumWarmConnections = 10))
      val reputationAggregator = mock[ReputationAggregatorActor[F]]

      val host1 = RemoteAddress("1", 1)
      val host2 = RemoteAddress("2", 2)
      val host3 = RemoteAddress("3", 3)
      val host4 = RemoteAddress("4", 4)
      val host5 = RemoteAddress("5", 5)
      val host6 = RemoteAddress("6", 6)

      (newPeerCreationAlgebra.requestNewPeerCreation _).expects(host1).returns(().pure[F])
      (newPeerCreationAlgebra.requestNewPeerCreation _).expects(host2).returns(().pure[F])

      val initialPeersMap: Map[HostId, Peer[F]] =
        Map(
          host1.host -> Peer(PeerState.Cold, None, Option(host1.port), Seq.empty, remoteNetworkLevel = true),
          host2.host -> Peer(PeerState.Cold, None, Option(host2.port), Seq.empty, remoteNetworkLevel = true),
          host3.host -> Peer(PeerState.Cold, None, None, Seq.empty, remoteNetworkLevel = true),
          host4.host -> Peer(PeerState.Warm, None, Option(host4.port), Seq.empty, remoteNetworkLevel = true),
          host5.host -> Peer(PeerState.Hot, None, Option(host5.port), Seq.empty, remoteNetworkLevel = true),
          host6.host -> Peer(PeerState.Banned, None, Option(host6.port), Seq.empty, remoteNetworkLevel = true)
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
          initialPeersMap,
          Caffeine.newBuilder.maximumSize(blockSourceCacheSize).build[BlockId, Set[HostId]]()
        )
        .use { actor =>
          for {
            _ <- actor.send(PeersManager.Message.SetupReputationAggregator(reputationAggregator))

            withUpdate <- actor.send(PeersManager.Message.UpdatedReputation(Map.empty, Map.empty, Map.empty))
            _ = assert(withUpdate.peers(host1.host).state == PeerState.Warm)
            _ = assert(withUpdate.peers(host2.host).state == PeerState.Warm)
            _ = assert(withUpdate.peers(host3.host).state == PeerState.Cold)
            _ = assert(withUpdate.peers(host4.host).state == PeerState.Warm)
            _ = assert(withUpdate.peers(host5.host).state == PeerState.Hot)
            _ = assert(withUpdate.peers(host6.host).state == PeerState.Banned)

          } yield ()
        }
    }
  }

  test("Reputation update: If no warm peer then move only eligible by timeout cold peer(s) to warm") {
    withMock {
      val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]
      val localChain: LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]]
      val slotDataStore: Store[F, BlockId, SlotData] = mock[Store[F, BlockId, SlotData]]
      val transactionStore: Store[F, TransactionId, IoTransaction] = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree: ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val p2pConfig: P2PNetworkConfig =
        defaultP2PConfig.copy(networkProperties = NetworkProperties(minimumWarmConnections = 5))
      val reputationAggregator = mock[ReputationAggregatorActor[F]]

      val host1 = RemoteAddress("1", 1)
      val host2 = RemoteAddress("2", 2)
      val host3 = RemoteAddress("3", 3)
      val host4 = RemoteAddress("4", 4)
      val host5 = RemoteAddress("5", 5)
      val host6 = RemoteAddress("5", 6)

      (newPeerCreationAlgebra.requestNewPeerCreation _).expects(host1).returns(().pure[F])
      (newPeerCreationAlgebra.requestNewPeerCreation _).expects(host2).returns(().pure[F])
      (newPeerCreationAlgebra.requestNewPeerCreation _).expects(host3).returns(().pure[F])

      val closeTimeoutFirstDelayInMs = 1000
      val currentTimestamp = System.currentTimeMillis()

      val eligibleTimestampFor1recentCloses = currentTimestamp - closeTimeoutFirstDelayInMs
      val eligibleTimestampFor2RecentCloses = currentTimestamp - (4 * closeTimeoutFirstDelayInMs)

      val initialPeersMap: Map[HostId, Peer[F]] =
        Map(
          host1.host -> Peer(PeerState.Cold, None, Option(host1.port), Seq.empty, remoteNetworkLevel = true),
          host2.host -> Peer(
            PeerState.Cold,
            None,
            Option(host2.port),
            Seq(eligibleTimestampFor1recentCloses),
            remoteNetworkLevel = true
          ),
          host3.host -> Peer(
            PeerState.Cold,
            None,
            Option(host3.port),
            Seq(0, eligibleTimestampFor2RecentCloses),
            remoteNetworkLevel = true
          ),
          host4.host -> Peer(
            PeerState.Cold,
            None,
            Option(host4.port),
            Seq(currentTimestamp),
            remoteNetworkLevel = true
          ),
          host5.host -> Peer(
            PeerState.Cold,
            None,
            Option(host5.port),
            Seq(currentTimestamp - closeTimeoutFirstDelayInMs),
            remoteNetworkLevel = true
          ),
          host6.host -> Peer(
            PeerState.Cold,
            None,
            Option(host6.port),
            Seq(0, 1, eligibleTimestampFor2RecentCloses),
            remoteNetworkLevel = true
          )
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
          new RandomColdToWarmSelector[F](closeTimeoutFirstDelayInMs),
          initialPeersMap,
          Caffeine.newBuilder.maximumSize(blockSourceCacheSize).build[BlockId, Set[HostId]]()
        )
        .use { actor =>
          for {
            _ <- actor.send(PeersManager.Message.SetupReputationAggregator(reputationAggregator))

            withUpdate <- actor.send(PeersManager.Message.UpdatedReputation(Map.empty, Map.empty, Map.empty))
            _ = assert(withUpdate.peers(host1.host).state == PeerState.Warm)
            _ = assert(withUpdate.peers(host2.host).state == PeerState.Warm)
            _ = assert(withUpdate.peers(host3.host).state == PeerState.Warm)
            _ = assert(withUpdate.peers(host4.host).state == PeerState.Cold)
            _ = assert(withUpdate.peers(host5.host).state == PeerState.Cold)
            _ = assert(withUpdate.peers(host6.host).state == PeerState.Cold)
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
        defaultP2PConfig.copy(networkProperties = NetworkProperties(minimumWarmConnections = 2))
      val reputationAggregator = mock[ReputationAggregatorActor[F]]

      val host1 = RemoteAddress("1", 1)
      val host2 = RemoteAddress("2", 2)
      val host3 = RemoteAddress("3", 3)
      val host4 = RemoteAddress("4", 4)

      val initialPeersMap: Map[HostId, Peer[F]] =
        Map(
          host1.host -> Peer(PeerState.Hot, None, Option(1), Seq.empty, remoteNetworkLevel = true),
          host2.host -> Peer(PeerState.Banned, None, None, Seq.empty, remoteNetworkLevel = true),
          host3.host -> Peer(PeerState.Cold, None, Option(3), Seq.empty, remoteNetworkLevel = true)
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
          initialPeersMap,
          Caffeine.newBuilder.maximumSize(blockSourceCacheSize).build[BlockId, Set[HostId]]()
        )
        .use { actor =>
          for {
            _            <- actor.send(PeersManager.Message.SetupReputationAggregator(reputationAggregator))
            withColdPeer <- actor.send(PeersManager.Message.AddKnownPeers(NonEmptyChain(host1, host2, host3, host4)))
            _ = assert(withColdPeer.peers(host1.host).state == PeerState.Hot)
            _ = assert(withColdPeer.peers(host2.host).state == PeerState.Banned)
            _ = assert(withColdPeer.peers(host3.host).state == PeerState.Cold)
            _ = assert(withColdPeer.peers(host4.host).state == PeerState.Cold)
            _ = assert(withColdPeer.peers(host4.host).closedTimestamps == Seq.empty)

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
      val reputationAggregator = mock[ReputationAggregatorActor[F]]

      val host1 = RemoteAddress("first", 1)
      val peer1 = mockPeerActor[F]()

      val host2 = RemoteAddress("second", 2)
      val peer2 = mockPeerActor[F]()

      val host3 = RemoteAddress("third", 3)
      val peer3 = mockPeerActor[F]()

      val host4 = RemoteAddress("fourth", 4)
      val peer4 = mockPeerActor[F]()

      val host5 = RemoteAddress("five", 5)
      val peer5 = mockPeerActor[F]()
      (peer5.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = false, applicationLevel = false))
        .returns(().pure[F])
      (peer5.sendNoWait _)
        .expects(PeerActor.Message.CloseConnection)
        .returns(().pure[F])

      val host6 = RemoteAddress("six", 6)
      val peer6 = mockPeerActor[F]()
      (peer6.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = false, applicationLevel = false))
        .returns(().pure[F])

      (reputationAggregator.sendNoWait _)
        .expects(ReputationAggregator.Message.StopReputationTracking(Set(host5.host, host6.host)))
        .returns(().pure[F])

      val initialPeersMap: Map[HostId, Peer[F]] =
        Map(
          host1.host -> Peer(PeerState.Hot, Option(peer1), Option(1), Seq(1), remoteNetworkLevel = true),
          host2.host -> Peer(PeerState.Hot, Option(peer2), None, Seq(2), remoteNetworkLevel = true),
          host3.host -> Peer(PeerState.Hot, Option(peer3), Option(3), Seq(3), remoteNetworkLevel = true),
          host4.host -> Peer(PeerState.Hot, Option(peer4), None, Seq(4), remoteNetworkLevel = true),
          host5.host -> Peer(PeerState.Hot, Option(peer5), None, Seq(5), remoteNetworkLevel = false),
          host6.host -> Peer(PeerState.Hot, Option(peer6), None, Seq(6), remoteNetworkLevel = true)
        )

      val performanceRep: Map[HostId, HostReputationValue] =
        Map(
          host1.host -> 1.0,
          host3.host -> defaultP2PConfig.networkProperties.minimumRequiredReputation * 1.05
        )

      val blockRep: Map[HostId, HostReputationValue] =
        Map(
          host2.host -> 1.0,
          host3.host -> defaultP2PConfig.networkProperties.minimumRequiredReputation * 1.05
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
          defaultP2PConfig,
          hotUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          initialPeersMap,
          Caffeine.newBuilder.maximumSize(blockSourceCacheSize).build[BlockId, Set[HostId]]()
        )
        .use { actor =>
          for {
            _          <- actor.send(PeersManager.Message.SetupReputationAggregator(reputationAggregator))
            withUpdate <- actor.send(PeersManager.Message.UpdatedReputation(performanceRep, blockRep, noveltyRep))
            _ = assert(withUpdate.peers(host1.host).state == PeerState.Hot)
            _ = assert(withUpdate.peers(host1.host).closedTimestamps == Seq(1))
            _ = assert(withUpdate.peers(host2.host).state == PeerState.Hot)
            _ = assert(withUpdate.peers(host2.host).closedTimestamps == Seq(2))
            _ = assert(withUpdate.peers(host3.host).state == PeerState.Hot)
            _ = assert(withUpdate.peers(host3.host).closedTimestamps == Seq(3))
            _ = assert(withUpdate.peers(host4.host).state == PeerState.Hot)
            _ = assert(withUpdate.peers(host4.host).closedTimestamps == Seq(4))
            _ = assert(withUpdate.peers(host5.host).state == PeerState.Cold)
            _ = assert(withUpdate.peers(host5.host).closedTimestamps.size == 2)
            _ = assert(withUpdate.peers(host6.host).state == PeerState.Cold)
            _ = assert(withUpdate.peers(host6.host).closedTimestamps.size == 2)
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
        defaultP2PConfig.copy(networkProperties = NetworkProperties(minimumHotConnections = 2))
      val reputationAggregator = mock[ReputationAggregatorActor[F]]

      val host1 = RemoteAddress("first", 1)
      val peer1 = mockPeerActor[F]()
      (peer1.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = true, applicationLevel = true))
        .returns(().pure[F])

      val host2 = RemoteAddress("second", 2)
      val peer2 = mockPeerActor[F]()
      (peer2.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = true, applicationLevel = true))
        .returns(().pure[F])

      val host3 = RemoteAddress("third", 3)
      val peer3 = mockPeerActor[F]()

      val host4 = RemoteAddress("fourth", 4)
      val peer4 = mockPeerActor[F]()

      val initialPeersMap: Map[HostId, Peer[F]] =
        Map(
          host1.host -> Peer(PeerState.Warm, Option(peer1), Option(1), Seq(1), remoteNetworkLevel = true),
          host2.host -> Peer(PeerState.Warm, Option(peer2), Option(2), Seq(2), remoteNetworkLevel = true),
          host3.host -> Peer(PeerState.Warm, Option(peer3), None, Seq(3), remoteNetworkLevel = true),
          host4.host -> Peer(PeerState.Warm, Option(peer4), None, Seq(4), remoteNetworkLevel = true)
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
          defaultColdToWarmSelector,
          initialPeersMap,
          Caffeine.newBuilder.maximumSize(blockSourceCacheSize).build[BlockId, Set[HostId]]()
        )
        .use { actor =>
          for {
            _          <- actor.send(PeersManager.Message.SetupReputationAggregator(reputationAggregator))
            withUpdate <- actor.send(PeersManager.Message.UpdatedReputation(performanceRep, Map.empty, Map.empty))
            _ = assert(withUpdate.peers(host1.host).state == PeerState.Hot)
            _ = assert(withUpdate.peers(host1.host).closedTimestamps == Seq(1))
            _ = assert(withUpdate.peers(host2.host).state == PeerState.Hot)
            _ = assert(withUpdate.peers(host2.host).closedTimestamps == Seq(2))
            _ = assert(withUpdate.peers(host3.host).state == PeerState.Warm)
            _ = assert(withUpdate.peers(host3.host).closedTimestamps == Seq(3))
            _ = assert(withUpdate.peers(host4.host).state == PeerState.Warm)
            _ = assert(withUpdate.peers(host4.host).closedTimestamps == Seq(4))
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
      val blockChecker = mock[BlockCheckerActor[F]]
      val reputationAggregator = mock[ReputationAggregatorActor[F]]
      val requestProxy = mock[RequestsProxyActor[F]]

      val host1 = RemoteAddress("1", 1)
      val peer1 = mockPeerActor[F]()
      (networkAlgebra.makePeer _).expects(host1.host, *, *, *, *, *, *, *, *, *, *).once().returns(Resource.pure(peer1))
      (peer1.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = true, applicationLevel = false))
        .returns(Applicative[F].unit)
      (peer1.sendNoWait _)
        .expects(PeerActor.Message.GetPeerServerAddress)
        .returns(Applicative[F].unit)

      val host2 = RemoteAddress("2", 2)
      val peer2 = mockPeerActor[F]()
      (networkAlgebra.makePeer _).expects(host2.host, *, *, *, *, *, *, *, *, *, *).once().returns(Resource.pure(peer2))
      (peer2.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = true, applicationLevel = false))
        .returns(Applicative[F].unit)
      (peer2.sendNoWait _)
        .expects(PeerActor.Message.GetPeerServerAddress)
        .returns(Applicative[F].unit)

      val host3 = RemoteAddress("3", 3)
      val peer3 = mockPeerActor[F]()
      (networkAlgebra.makePeer _).expects(host3.host, *, *, *, *, *, *, *, *, *, *).once().returns(Resource.pure(peer3))
      (peer3.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = true, applicationLevel = false))
        .returns(Applicative[F].unit)
      (peer3.sendNoWait _)
        .expects(PeerActor.Message.GetPeerServerAddress)
        .returns(Applicative[F].unit)

      val host4 = RemoteAddress("4", 4)
      val host5 = RemoteAddress("5", 5)

      val initialPeersMap = Map(
        host1.host -> Peer(PeerState.Warm, None, None, Seq(1), remoteNetworkLevel = true),
        host3.host -> Peer(PeerState.Warm, None, None, Seq(3), remoteNetworkLevel = true),
        host4.host -> Peer(PeerState.Banned, None, None, Seq(4), remoteNetworkLevel = true),
        host5.host -> Peer(PeerState.Hot, None, None, Seq(5), remoteNetworkLevel = true)
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
          defaultP2PConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          initialPeersMap,
          Caffeine.newBuilder.maximumSize(blockSourceCacheSize).build[BlockId, Set[HostId]]()
        )
        .use { actor =>
          for {
            _          <- actor.send(PeersManager.Message.SetupReputationAggregator(reputationAggregator))
            _          <- actor.send(PeersManager.Message.SetupBlockChecker(blockChecker))
            _          <- actor.send(PeersManager.Message.SetupRequestsProxy(requestProxy))
            stateHost1 <- actor.send(PeersManager.Message.OpenedPeerConnection(host1, mock[BlockchainPeerClient[F]]))
            _ = assert(stateHost1.peers(host1.host).state == PeerState.Warm)
            _ = assert(stateHost1.peers(host1.host).closedTimestamps == Seq(1))
            stateHost2 <- actor.send(PeersManager.Message.OpenedPeerConnection(host2, mock[BlockchainPeerClient[F]]))
            _ = assert(stateHost2.peers(host2.host).state == PeerState.Warm)
            _ = assert(stateHost2.peers(host2.host).closedTimestamps == Seq.empty)
            stateHost3 <- actor.send(PeersManager.Message.OpenedPeerConnection(host3, mock[BlockchainPeerClient[F]]))
            _ = assert(stateHost3.peers(host3.host).state == PeerState.Warm)
            _ = assert(stateHost3.peers(host3.host).closedTimestamps == Seq(3))
            stateHost4 <- actor.send(PeersManager.Message.OpenedPeerConnection(host4, mock[BlockchainPeerClient[F]]))
            _ = assert(stateHost4.peers(host4.host).state == PeerState.Banned)
            _ = assert(stateHost4.peers(host4.host).closedTimestamps == Seq(4))
            stateHost5 <- actor.send(PeersManager.Message.OpenedPeerConnection(host5, mock[BlockchainPeerClient[F]]))
            _ = assert(stateHost5.peers(host5.host).state == PeerState.Hot)
            _ = assert(stateHost5.peers(host5.host).closedTimestamps == Seq(5))
          } yield ()
        }
    }
  }

  test("Receiving remote network level shall update it and sometimes close connection") {
    withMock {
      val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]
      val localChain: LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]]
      val slotDataStore: Store[F, BlockId, SlotData] = mock[Store[F, BlockId, SlotData]]
      val transactionStore: Store[F, TransactionId, IoTransaction] = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree: ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val blockChecker = mock[BlockCheckerActor[F]]
      val reputationAggregator = mock[ReputationAggregatorActor[F]]
      val requestProxy = mock[RequestsProxyActor[F]]

      val host1 = RemoteAddress("1", 1)
      val peer1 = mockPeerActor[F]()
      (peer1.sendNoWait _)
        .expects(PeerActor.Message.CloseConnection)
        .returns(Applicative[F].unit)

      val host2 = RemoteAddress("2", 2)
      val peer2 = mockPeerActor[F]()
      (peer2.sendNoWait _)
        .expects(PeerActor.Message.CloseConnection)
        .returns(Applicative[F].unit)

      val host3 = RemoteAddress("3", 3)
      val peer3 = mockPeerActor[F]()

      val host4 = RemoteAddress("4", 4)
      val peer4 = mockPeerActor[F]()

      val initialPeersMap = Map(
        host1.host -> Peer(PeerState.Banned, Option(peer1), None, Seq(1), remoteNetworkLevel = true),
        host2.host -> Peer(PeerState.Cold, Option(peer2), None, Seq(3), remoteNetworkLevel = true),
        host3.host -> Peer(PeerState.Warm, Option(peer3), None, Seq(4), remoteNetworkLevel = true),
        host4.host -> Peer(PeerState.Hot, Option(peer4), None, Seq(5), remoteNetworkLevel = true)
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
          defaultP2PConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          initialPeersMap,
          Caffeine.newBuilder.maximumSize(blockSourceCacheSize).build[BlockId, Set[HostId]]()
        )
        .use { actor =>
          for {
            _          <- actor.send(PeersManager.Message.SetupReputationAggregator(reputationAggregator))
            _          <- actor.send(PeersManager.Message.SetupBlockChecker(blockChecker))
            _          <- actor.send(PeersManager.Message.SetupRequestsProxy(requestProxy))
            stateHost1 <- actor.send(PeersManager.Message.RemotePeerNetworkLevel(host1.host, networkLevel = false))
            _ = assert(stateHost1.peers(host1.host).state == PeerState.Banned)
            _ = assert(!stateHost1.peers(host1.host).remoteNetworkLevel)
            stateHost2 <- actor.send(PeersManager.Message.RemotePeerNetworkLevel(host2.host, networkLevel = false))
            _ = assert(stateHost2.peers(host2.host).state == PeerState.Cold)
            _ = assert(!stateHost2.peers(host2.host).remoteNetworkLevel)
            stateHost3 <- actor.send(PeersManager.Message.RemotePeerNetworkLevel(host3.host, networkLevel = false))
            _ = assert(stateHost3.peers(host3.host).state == PeerState.Warm)
            _ = assert(!stateHost3.peers(host3.host).remoteNetworkLevel)
            stateHost4 <- actor.send(PeersManager.Message.RemotePeerNetworkLevel(host4.host, networkLevel = false))
            _ = assert(stateHost4.peers(host4.host).state == PeerState.Hot)
            _ = assert(!stateHost4.peers(host4.host).remoteNetworkLevel)
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
      val blockChecker = mock[BlockCheckerActor[F]]
      val reputationAggregator = mock[ReputationAggregatorActor[F]]
      val requestProxy = mock[RequestsProxyActor[F]]

      val host1 = RemoteAddress("first", 1)
      val peer1 = mockPeerActor[F]()
      val hostServer1Port = 999
      val host1serverAddress = RemoteAddress(host1.host, hostServer1Port)

      val host2 = RemoteAddress("second", 2)
      val hostServer2 = RemoteAddress("secondServer", 1)

      val host3 = RemoteAddress("third", 3)
      val peer3 = mockPeerActor[F]()

      val host4 = RemoteAddress("fourth", 4)
      val peer4 = mockPeerActor[F]()
      val hostServer4Port = 999
      val host4serverAddress = RemoteAddress(host4.host, hostServer4Port)

      val host5 = RemoteAddress("five", 5)
      val peer5 = mockPeerActor[F]()
      val hostServer5 = RemoteAddress("fiveServer", 5)

      val initialPeersMap: Map[HostId, Peer[F]] =
        Map(
          host1.host -> Peer(PeerState.Hot, Option(peer1), None, Seq(1), remoteNetworkLevel = true),
          host3.host -> Peer(PeerState.Hot, Option(peer3), None, Seq(3), remoteNetworkLevel = true),
          host4.host -> Peer(
            PeerState.Hot,
            Option(peer4),
            Option(host4serverAddress.port),
            Seq(4),
            remoteNetworkLevel = true
          ),
          host5.host -> Peer(PeerState.Warm, Option(peer5), Option(hostServer5.port), Seq(5), remoteNetworkLevel = true)
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
          defaultP2PConfig,
          hotUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          initialPeersMap,
          Caffeine.newBuilder.maximumSize(blockSourceCacheSize).build[BlockId, Set[HostId]]()
        )
        .use { actor =>
          for {
            _         <- actor.send(PeersManager.Message.SetupReputationAggregator(reputationAggregator))
            _         <- actor.send(PeersManager.Message.SetupBlockChecker(blockChecker))
            _         <- actor.send(PeersManager.Message.SetupRequestsProxy(requestProxy))
            newState1 <- actor.send(PeersManager.Message.RemotePeerServerPort(host1.host, hostServer1Port))
            _ = assert(newState1.peers(host1.host).remoteServerPort == Option(host1serverAddress.port))
            _ = assert(newState1.peers(host1.host).closedTimestamps == Seq(1))
            newState2 <- actor.send(PeersManager.Message.RemotePeerServerPort(host2.host, hostServer2.port))
            _ = assert(newState1.peers == newState2.peers)
            _ = assert(newState2.peers(host5.host).closedTimestamps == Seq(5))
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
          host1 -> Peer(PeerState.Banned, None, Option(hostServer1Port), Seq.empty, remoteNetworkLevel = true),
          host2 -> Peer(PeerState.Cold, None, Option(hostServer2Port), Seq.empty, remoteNetworkLevel = true),
          host3 -> Peer(PeerState.Warm, None, Option(hostServer3Port), Seq.empty, remoteNetworkLevel = true),
          host4 -> Peer(PeerState.Warm, None, Option(hostServer4Port), Seq.empty, remoteNetworkLevel = true),
          host5 -> Peer(PeerState.Hot, None, Option(hostServer5Port), Seq.empty, remoteNetworkLevel = true),
          host6 -> Peer(PeerState.Hot, None, None, Seq.empty, remoteNetworkLevel = true)
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
          defaultP2PConfig,
          defaultHotPeerUpdater,
          writingHosts,
          defaultColdToWarmSelector,
          initialPeersMap,
          Caffeine.newBuilder.maximumSize(blockSourceCacheSize).build[BlockId, Set[HostId]]()
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

  test("Block source shall be sent to reputation aggregator with correct count") {
    PropF.forAllF(nonEmptyChainArbOfLen(arbitraryHostBlockId, maxChainSize).arbitrary) {
      inputData: NonEmptyChain[(HostId, BlockId)] =>
        withMock {
          val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]
          val localChain: LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]]
          val slotDataStore: Store[F, BlockId, SlotData] = mock[Store[F, BlockId, SlotData]]
          val transactionStore: Store[F, TransactionId, IoTransaction] = mock[Store[F, TransactionId, IoTransaction]]
          val blockIdTree: ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]
          val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] =
            mock[BlockHeaderToBodyValidationAlgebra[F]]
          val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
          val blockChecker = mock[BlockCheckerActor[F]]
          val reputationAggregator = mock[ReputationAggregatorActor[F]]
          val requestProxy = mock[RequestsProxyActor[F]]

          val blockWithSource: Map[BlockId, Set[HostId]] =
            inputData.toList
              .take(10)
              .map(d => d._2)
              .zipWithIndex
              .map { case (block, index) =>
                val sourcesForBlock = (1 to index).map(i => i.toString).toSet
                (block, sourcesForBlock)
              }
              .toMap

          val initialCash =
            Caffeine.newBuilder.maximumSize(blockSourceCacheSize).build[BlockId, Set[HostId]]()
          initialCash.putAll(blockWithSource.asJava)

          val expectedData = inputData.map { case (host, blockId) =>
            (host, blockWithSource.getOrElse(blockId, Set.empty).size.toLong + 1)
          }

          (reputationAggregator.sendNoWait _)
            .expects(ReputationAggregator.Message.BlockProvidingReputationUpdate(expectedData))
            .once()
            .returns(().pure[F])

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
              defaultP2PConfig,
              defaultHotPeerUpdater,
              defaultPeersSaver,
              defaultColdToWarmSelector,
              Map.empty,
              initialCash
            )
            .use { actor =>
              for {
                _ <- actor.send(PeersManager.Message.SetupReputationAggregator(reputationAggregator))
                _ <- actor.send(PeersManager.Message.SetupBlockChecker(blockChecker))
                _ <- actor.send(PeersManager.Message.SetupRequestsProxy(requestProxy))
                _ <- actor.send(PeersManager.Message.BlocksSource(inputData))
                // if we send the same data no message is sent
                _ <- actor.send(PeersManager.Message.BlocksSource(inputData))
              } yield ()
            }
        }
    }
  }

  test("Add known neighbours shall correctly filter out loopback IP addresses") {
    withMock {
      val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]
      val localChain: LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]]
      val slotDataStore: Store[F, BlockId, SlotData] = mock[Store[F, BlockId, SlotData]]
      val transactionStore: Store[F, TransactionId, IoTransaction] = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree: ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] =
        mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val blockChecker = mock[BlockCheckerActor[F]]
      val reputationAggregator = mock[ReputationAggregatorActor[F]]
      val requestProxy = mock[RequestsProxyActor[F]]

      val externalIPs = Set("126.0.0.0", "8.8.8.8")
      val specialIPs =
        Set("0.0.0.0", "127.127.127.127", "238.255.255.255", "224.0.0.0", "0.0.0.0", "255.255.255.255")

      val remoteAddresses =
        NonEmptyChain.fromSeq((externalIPs ++ specialIPs).map(ip => RemoteAddress(ip, 0)).toSeq).get
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
          defaultP2PConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          Map.empty,
          Caffeine.newBuilder.maximumSize(blockSourceCacheSize).build[BlockId, Set[HostId]]()
        )
        .use { actor =>
          for {
            _            <- actor.send(PeersManager.Message.SetupReputationAggregator(reputationAggregator))
            _            <- actor.send(PeersManager.Message.SetupBlockChecker(blockChecker))
            _            <- actor.send(PeersManager.Message.SetupRequestsProxy(requestProxy))
            updatedState <- actor.send(PeersManager.Message.AddKnownNeighbors(remoteAddresses))
            knownPeers1 = updatedState.peers.peers.keySet
            _ = assert(externalIPs.forall(knownPeers1.contains))
            _ = assert(specialIPs.forall(!knownPeers1.contains(_)))
            updateWithAddKnown <- actor.send(PeersManager.Message.AddKnownPeers(remoteAddresses))
            knownPeers2 = updateWithAddKnown.peers.peers.keySet
            _ = assert(externalIPs.forall(knownPeers2.contains))
            _ = assert(specialIPs.forall(knownPeers2.contains))
          } yield ()
        }
    }

  }

  test("Request for block header download shall be sent to one of the peers") {
    withMock {
      val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]
      val localChain: LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]]
      val slotDataStore: Store[F, BlockId, SlotData] = mock[Store[F, BlockId, SlotData]]
      val transactionStore: Store[F, TransactionId, IoTransaction] = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree: ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] =
        mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val blockChecker = mock[BlockCheckerActor[F]]
      val reputationAggregator = mock[ReputationAggregatorActor[F]]
      val requestProxy = mock[RequestsProxyActor[F]]

      val blockHeader1 = ModelGenerators.arbitraryHeader.arbitrary.first.embedId
      val blockHeader2 = ModelGenerators.arbitraryHeader.arbitrary.first.embedId

      val blockSource: HostId = arbitraryHost.arbitrary.first
      val warmPeer = arbitraryHost.arbitrary.first
      val coldPeer = arbitraryHost.arbitrary.first
      val preWarmPeer = arbitraryHost.arbitrary.first
      val bannedPeer = arbitraryHost.arbitrary.first

      val blockWithSource: Map[BlockId, Set[HostId]] =
        Map(
          blockHeader1.id -> Set(warmPeer, blockSource, coldPeer, preWarmPeer, bannedPeer),
          blockHeader2.id -> Set(blockSource, warmPeer, coldPeer, preWarmPeer, bannedPeer)
        )

      val initialCash =
        Caffeine.newBuilder.maximumSize(blockSourceCacheSize).build[BlockId, Set[HostId]]()
      initialCash.putAll(blockWithSource.asJava)

      val peer: PeerActor[F] = mockPeerActor[F]()
      val initialPeersMap: Map[HostId, Peer[F]] = Map(
        blockSource -> Peer(PeerState.Hot, Option(peer), None, Seq.empty, remoteNetworkLevel = true),
        coldPeer    -> Peer(PeerState.Cold, Option(mockPeerActor[F]()), None, Seq.empty, remoteNetworkLevel = true),
        preWarmPeer -> Peer(PeerState.Warm, Option(mockPeerActor[F]()), None, Seq.empty, remoteNetworkLevel = true),
        bannedPeer  -> Peer(PeerState.Banned, Option(mockPeerActor[F]()), None, Seq.empty, remoteNetworkLevel = true),
        warmPeer    -> Peer(PeerState.Warm, Option(mockPeerActor[F]()), None, Seq.empty, remoteNetworkLevel = true)
      )

      val expectedMessage = PeerActor.Message.DownloadBlockHeaders(NonEmptyChain(blockHeader1.id, blockHeader2.id))
      (peer.sendNoWait _).expects(expectedMessage).once().returns(Applicative[F].unit)

      val messageToSend =
        PeersManager.Message.BlockHeadersRequest(None, NonEmptyChain(blockHeader1.id, blockHeader2.id))

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
          defaultP2PConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          initialPeersMap,
          initialCash
        )
        .use { actor =>
          for {
            _ <- actor.send(PeersManager.Message.SetupReputationAggregator(reputationAggregator))
            _ <- actor.send(PeersManager.Message.SetupBlockChecker(blockChecker))
            _ <- actor.send(PeersManager.Message.SetupRequestsProxy(requestProxy))
            _ <- actor.send(messageToSend)
          } yield ()
        }
    }

  }

  test("Request for block header download shall be sent to one of the peers if only hint is available") {
    withMock {
      val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]
      val localChain: LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]]
      val slotDataStore: Store[F, BlockId, SlotData] = mock[Store[F, BlockId, SlotData]]
      val transactionStore: Store[F, TransactionId, IoTransaction] = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree: ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] =
        mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val blockChecker = mock[BlockCheckerActor[F]]
      val reputationAggregator = mock[ReputationAggregatorActor[F]]
      val requestProxy = mock[RequestsProxyActor[F]]

      val blockHeader1 = ModelGenerators.arbitraryHeader.arbitrary.first.embedId
      val blockHeader2 = ModelGenerators.arbitraryHeader.arbitrary.first.embedId

      val blockSource: HostId = arbitraryHost.arbitrary.first

      val blockWithSource: Map[BlockId, Set[HostId]] = Map()

      val initialCash =
        Caffeine.newBuilder.maximumSize(blockSourceCacheSize).build[BlockId, Set[HostId]]()
      initialCash.putAll(blockWithSource.asJava)

      val peer: PeerActor[F] = mockPeerActor[F]()
      val initialPeersMap: Map[HostId, Peer[F]] = Map(
        blockSource -> Peer(PeerState.Hot, Option(peer), None, Seq.empty, remoteNetworkLevel = true)
      )

      val expectedMessage = PeerActor.Message.DownloadBlockHeaders(NonEmptyChain(blockHeader1.id, blockHeader2.id))
      (peer.sendNoWait _).expects(expectedMessage).once().returns(Applicative[F].unit)

      val messageToSend =
        PeersManager.Message.BlockHeadersRequest(Option(blockSource), NonEmptyChain(blockHeader1.id, blockHeader2.id))

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
          defaultP2PConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          initialPeersMap,
          initialCash
        )
        .use { actor =>
          for {
            _ <- actor.send(PeersManager.Message.SetupReputationAggregator(reputationAggregator))
            _ <- actor.send(PeersManager.Message.SetupBlockChecker(blockChecker))
            _ <- actor.send(PeersManager.Message.SetupRequestsProxy(requestProxy))
            _ <- actor.send(messageToSend)
          } yield ()
        }

    }
  }

  test("Request for block header download shall not be sent to one of the peers because of bad peer state") {
    withMock {
      val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]
      val localChain: LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]]
      val slotDataStore: Store[F, BlockId, SlotData] = mock[Store[F, BlockId, SlotData]]
      val transactionStore: Store[F, TransactionId, IoTransaction] = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree: ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] =
        mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val blockChecker = mock[BlockCheckerActor[F]]
      val reputationAggregator = mock[ReputationAggregatorActor[F]]
      val requestProxy = mock[RequestsProxyActor[F]]

      val blockHeader1 = ModelGenerators.arbitraryHeader.arbitrary.first.embedId
      val blockHeader2 = ModelGenerators.arbitraryHeader.arbitrary.first.embedId

      val warmPeer = arbitraryHost.arbitrary.first
      val coldPeer = arbitraryHost.arbitrary.first
      val preWarmPeer = arbitraryHost.arbitrary.first
      val bannedPeer = arbitraryHost.arbitrary.first

      val blockWithSource: Map[BlockId, Set[HostId]] =
        Map(
          blockHeader1.id -> Set(warmPeer, coldPeer, preWarmPeer, bannedPeer),
          blockHeader2.id -> Set(warmPeer, coldPeer, preWarmPeer, bannedPeer)
        )

      val initialCash =
        Caffeine.newBuilder.maximumSize(blockSourceCacheSize).build[BlockId, Set[HostId]]()
      initialCash.putAll(blockWithSource.asJava)

      val initialPeersMap: Map[HostId, Peer[F]] = Map(
        coldPeer    -> Peer(PeerState.Cold, Option(mockPeerActor[F]()), None, Seq.empty, remoteNetworkLevel = true),
        preWarmPeer -> Peer(PeerState.Warm, Option(mockPeerActor[F]()), None, Seq.empty, remoteNetworkLevel = true),
        bannedPeer  -> Peer(PeerState.Banned, Option(mockPeerActor[F]()), None, Seq.empty, remoteNetworkLevel = true),
        warmPeer    -> Peer(PeerState.Warm, Option(mockPeerActor[F]()), None, Seq.empty, remoteNetworkLevel = true)
      )

      val expectedMessage = BlockChecker.Message.InvalidateBlockIds(NonEmptyChain(blockHeader2.id))
      (blockChecker.sendNoWait _).expects(expectedMessage).once().returns(Applicative[F].unit)

      val messageToSend =
        PeersManager.Message.BlockHeadersRequest(
          Option(arbitraryHost.arbitrary.first),
          NonEmptyChain(blockHeader1.id, blockHeader2.id)
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
          defaultP2PConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          initialPeersMap,
          initialCash
        )
        .use { actor =>
          for {
            _ <- actor.send(PeersManager.Message.SetupReputationAggregator(reputationAggregator))
            _ <- actor.send(PeersManager.Message.SetupBlockChecker(blockChecker))
            _ <- actor.send(PeersManager.Message.SetupRequestsProxy(requestProxy))
            _ <- actor.send(messageToSend)
          } yield ()
        }

    }
  }

  test("Request for block download shall be sent to one of the peers") {
    withMock {
      val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]
      val localChain: LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]]
      val slotDataStore: Store[F, BlockId, SlotData] = mock[Store[F, BlockId, SlotData]]
      val transactionStore: Store[F, TransactionId, IoTransaction] = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree: ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] =
        mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val blockChecker = mock[BlockCheckerActor[F]]
      val reputationAggregator = mock[ReputationAggregatorActor[F]]
      val requestProxy = mock[RequestsProxyActor[F]]

      val blockHeader1 = ModelGenerators.arbitraryHeader.arbitrary.first.embedId
      val blockHeader2 = ModelGenerators.arbitraryHeader.arbitrary.first.embedId

      val blockSource: HostId = arbitraryHost.arbitrary.first
      val warmPeer = arbitraryHost.arbitrary.first
      val coldPeer = arbitraryHost.arbitrary.first
      val preWarmPeer = arbitraryHost.arbitrary.first
      val bannedPeer = arbitraryHost.arbitrary.first

      val blockWithSource: Map[BlockId, Set[HostId]] =
        Map(
          blockHeader1.id -> Set(warmPeer, blockSource, coldPeer, preWarmPeer, bannedPeer),
          blockHeader2.id -> Set(blockSource, warmPeer, coldPeer, preWarmPeer, bannedPeer)
        )

      val initialCash =
        Caffeine.newBuilder.maximumSize(blockSourceCacheSize).build[BlockId, Set[HostId]]()
      initialCash.putAll(blockWithSource.asJava)

      val peer: PeerActor[F] = mockPeerActor[F]()
      val initialPeersMap: Map[HostId, Peer[F]] = Map(
        blockSource -> Peer(PeerState.Hot, Option(peer), None, Seq.empty, remoteNetworkLevel = true),
        coldPeer    -> Peer(PeerState.Cold, Option(mockPeerActor[F]()), None, Seq.empty, remoteNetworkLevel = true),
        preWarmPeer -> Peer(PeerState.Warm, Option(mockPeerActor[F]()), None, Seq.empty, remoteNetworkLevel = true),
        bannedPeer  -> Peer(PeerState.Banned, Option(mockPeerActor[F]()), None, Seq.empty, remoteNetworkLevel = true),
        warmPeer    -> Peer(PeerState.Warm, Option(mockPeerActor[F]()), None, Seq.empty, remoteNetworkLevel = true)
      )

      val expectedMessage = PeerActor.Message.DownloadBlockBodies(NonEmptyChain(blockHeader1, blockHeader2))
      (peer.sendNoWait _).expects(expectedMessage).once().returns(Applicative[F].unit)

      val messageToSend =
        PeersManager.Message.BlockBodyRequest(None, NonEmptyChain(blockHeader1, blockHeader2))

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
          defaultP2PConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          initialPeersMap,
          initialCash
        )
        .use { actor =>
          for {
            _ <- actor.send(PeersManager.Message.SetupReputationAggregator(reputationAggregator))
            _ <- actor.send(PeersManager.Message.SetupBlockChecker(blockChecker))
            _ <- actor.send(PeersManager.Message.SetupRequestsProxy(requestProxy))
            _ <- actor.send(messageToSend)
          } yield ()
        }

    }
  }

  test("Request for block download shall be sent to one of the peers if only hint is available") {
    withMock {
      val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]
      val localChain: LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]]
      val slotDataStore: Store[F, BlockId, SlotData] = mock[Store[F, BlockId, SlotData]]
      val transactionStore: Store[F, TransactionId, IoTransaction] = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree: ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] =
        mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val blockChecker = mock[BlockCheckerActor[F]]
      val reputationAggregator = mock[ReputationAggregatorActor[F]]
      val requestProxy = mock[RequestsProxyActor[F]]

      val blockHeader1 = ModelGenerators.arbitraryHeader.arbitrary.first.embedId
      val blockHeader2 = ModelGenerators.arbitraryHeader.arbitrary.first.embedId

      val blockSource: HostId = arbitraryHost.arbitrary.first

      val blockWithSource: Map[BlockId, Set[HostId]] = Map()

      val initialCash =
        Caffeine.newBuilder.maximumSize(blockSourceCacheSize).build[BlockId, Set[HostId]]()
      initialCash.putAll(blockWithSource.asJava)

      val peer: PeerActor[F] = mockPeerActor[F]()
      val initialPeersMap: Map[HostId, Peer[F]] = Map(
        blockSource -> Peer(PeerState.Hot, Option(peer), None, Seq.empty, remoteNetworkLevel = true)
      )

      val expectedMessage = PeerActor.Message.DownloadBlockBodies(NonEmptyChain(blockHeader1, blockHeader2))
      (peer.sendNoWait _).expects(expectedMessage).once().returns(Applicative[F].unit)

      val messageToSend =
        PeersManager.Message.BlockBodyRequest(Option(blockSource), NonEmptyChain(blockHeader1, blockHeader2))

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
          defaultP2PConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          initialPeersMap,
          initialCash
        )
        .use { actor =>
          for {
            _ <- actor.send(PeersManager.Message.SetupReputationAggregator(reputationAggregator))
            _ <- actor.send(PeersManager.Message.SetupBlockChecker(blockChecker))
            _ <- actor.send(PeersManager.Message.SetupRequestsProxy(requestProxy))
            _ <- actor.send(messageToSend)
          } yield ()
        }

    }
  }

  test("Request for block download shall not be sent to one of the peers because of bad peer state") {
    withMock {
      val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]
      val localChain: LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]]
      val slotDataStore: Store[F, BlockId, SlotData] = mock[Store[F, BlockId, SlotData]]
      val transactionStore: Store[F, TransactionId, IoTransaction] = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree: ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] =
        mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val blockChecker = mock[BlockCheckerActor[F]]
      val reputationAggregator = mock[ReputationAggregatorActor[F]]
      val requestProxy = mock[RequestsProxyActor[F]]

      val blockHeader1 = ModelGenerators.arbitraryHeader.arbitrary.first.embedId
      val blockHeader2 = ModelGenerators.arbitraryHeader.arbitrary.first.embedId

      val warmPeer = arbitraryHost.arbitrary.first
      val coldPeer = arbitraryHost.arbitrary.first
      val preWarmPeer = arbitraryHost.arbitrary.first
      val bannedPeer = arbitraryHost.arbitrary.first

      val blockWithSource: Map[BlockId, Set[HostId]] =
        Map(
          blockHeader1.id -> Set(warmPeer, coldPeer, preWarmPeer, bannedPeer),
          blockHeader2.id -> Set(warmPeer, coldPeer, preWarmPeer, bannedPeer)
        )

      val initialCash =
        Caffeine.newBuilder.maximumSize(blockSourceCacheSize).build[BlockId, Set[HostId]]()
      initialCash.putAll(blockWithSource.asJava)

      val initialPeersMap: Map[HostId, Peer[F]] = Map(
        coldPeer    -> Peer(PeerState.Cold, Option(mockPeerActor[F]()), None, Seq.empty, remoteNetworkLevel = true),
        preWarmPeer -> Peer(PeerState.Warm, Option(mockPeerActor[F]()), None, Seq.empty, remoteNetworkLevel = true),
        bannedPeer  -> Peer(PeerState.Banned, Option(mockPeerActor[F]()), None, Seq.empty, remoteNetworkLevel = true),
        warmPeer    -> Peer(PeerState.Warm, Option(mockPeerActor[F]()), None, Seq.empty, remoteNetworkLevel = true)
      )

      val expectedMessage = BlockChecker.Message.InvalidateBlockIds(NonEmptyChain(blockHeader2.id))
      (blockChecker.sendNoWait _).expects(expectedMessage).once().returns(Applicative[F].unit)

      val messageToSend =
        PeersManager.Message.BlockBodyRequest(
          Option(arbitraryHost.arbitrary.first),
          NonEmptyChain(blockHeader1, blockHeader2)
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
          defaultP2PConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          initialPeersMap,
          initialCash
        )
        .use { actor =>
          for {
            _ <- actor.send(PeersManager.Message.SetupReputationAggregator(reputationAggregator))
            _ <- actor.send(PeersManager.Message.SetupBlockChecker(blockChecker))
            _ <- actor.send(PeersManager.Message.SetupRequestsProxy(requestProxy))
            _ <- actor.send(messageToSend)
          } yield ()
        }

    }
  }
}
