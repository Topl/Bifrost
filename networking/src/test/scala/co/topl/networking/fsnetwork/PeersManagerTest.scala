package co.topl.networking.fsnetwork

import cats.Applicative
import cats.data.NonEmptyChain
import cats.effect.kernel.Sync
import cats.effect.{Async, IO, Resource}
import cats.implicits._
import co.topl.algebras.Store
import co.topl.brambl.generators.TransactionGenerator
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.validation.algebras.TransactionSyntaxVerifier
import co.topl.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import co.topl.config.ApplicationConfig.Bifrost.NetworkProperties
import co.topl.consensus.algebras.{BlockHeaderToBodyValidationAlgebra, LocalChainAlgebra}
import co.topl.consensus.models.{BlockId, SlotData}
import co.topl.eventtree.{EventSourcedState, ParentChildTree}
import co.topl.ledger.algebras.MempoolAlgebra
import co.topl.models.ModelGenerators.GenHelper
import co.topl.models.generators.consensus.ModelGenerators
import co.topl.models.generators.consensus.ModelGenerators.arbitraryBlockId
import co.topl.networking.blockchain.BlockchainPeerClient
import co.topl.networking.fsnetwork.BlockChecker.BlockCheckerActor
import co.topl.networking.fsnetwork.NetworkQualityError.{IncorrectPongMessage, NoPongMessage}
import co.topl.networking.fsnetwork.PeerActor.PeerActor
import co.topl.networking.fsnetwork.PeersManagerTest.F
import co.topl.networking.fsnetwork.RequestsProxy.RequestsProxyActor
import co.topl.networking.fsnetwork.TestHelper.arbitraryHost
import co.topl.networking.p2p.RemoteAddress
import com.github.benmanes.caffeine.cache.{Cache, Caffeine}
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

  val thisHostId: HostId = "43.0.0.0"
  val hostId: HostId = "127.0.0.1"

  val defaultColdToWarmSelector: SelectorColdToWarm[F] =
    (coldHosts: Map[HostId, Peer[F]], countToReceive: Int) =>
      coldHosts.toSeq.sortBy(_._2.remoteServerPort).take(countToReceive).map(_._1).toSet

  val defaultWarmToHotSelector: SelectorWarmToHot[F] =
    new SelectorWarmToHot[F]() {

      override def select(hosts: Map[HostId, Peer[F]], countToReceive: Int): Set[HostId] =
        hosts.toSeq.sortBy(_._2.reputation).takeRight(countToReceive).map(_._1).toSet
    }

  val defaultHotPeerUpdater: Set[RemoteAddress] => F[Unit] = _ => Applicative[F].unit
  val defaultPeersSaver: Set[RemotePeer] => F[Unit] = _ => Applicative[F].unit

  val defaultP2PConfig: P2PNetworkConfig =
    P2PNetworkConfig(NetworkProperties(closeTimeoutWindowInMs = Long.MaxValue), FiniteDuration(1, SECONDS))

  val defaultTransactionSyntaxValidation: TransactionSyntaxVerifier[F] = (t: IoTransaction) => Either.right(t).pure[F]

  def defaultCache(): Cache[BlockId, Set[HostId]] =
    Caffeine.newBuilder.maximumSize(blockSourceCacheSize).build[BlockId, Set[HostId]]()

  implicit val dummyDns: DnsResolver[F] = (host: HostId) => Option(host).pure[F]
  implicit val dummyReverseDns: ReverseDnsResolver[F] = (h: HostId) => h.pure[F]

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
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val mempool = mock[MempoolAlgebra[F]]

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
          coldHost -> Peer(
            PeerState.Cold,
            Option(coldPeer),
            coldHost,
            None,
            Seq.empty,
            remoteNetworkLevel = true,
            0,
            0,
            0
          ),
          warmHost -> Peer(
            PeerState.Warm,
            Option(warmPeer),
            warmHost,
            None,
            Seq.empty,
            remoteNetworkLevel = true,
            0,
            0,
            0
          ),
          hotHost -> Peer(PeerState.Hot, Option(hotPeer), hotHost, None, Seq.empty, remoteNetworkLevel = true, 0, 0, 0),
          banedHost -> Peer(
            PeerState.Banned,
            Option(banedPeer),
            banedHost,
            None,
            Seq.empty,
            remoteNetworkLevel = true,
            0,
            0,
            0
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
          blockHeights,
          mempool,
          headerToBodyValidation,
          defaultTransactionSyntaxValidation,
          newPeerCreationAlgebra,
          defaultP2PConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          defaultWarmToHotSelector,
          initialPeersMap,
          defaultCache()
        )
        .use { actor =>
          for {
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
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val mempool = mock[MempoolAlgebra[F]]

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
          coldHost -> Peer(
            PeerState.Cold,
            Option(coldPeer),
            coldHost,
            None,
            Seq.empty,
            remoteNetworkLevel = true,
            0,
            0,
            0
          ),
          warmHost -> Peer(
            PeerState.Warm,
            Option(warmPeer),
            warmHost,
            None,
            Seq.empty,
            remoteNetworkLevel = true,
            0,
            0,
            0
          ),
          hotHost -> Peer(PeerState.Hot, Option(hotPeer), hotHost, None, Seq.empty, remoteNetworkLevel = true, 0, 0, 0),
          banedHost -> Peer(
            PeerState.Banned,
            Option(banedPeer),
            banedHost,
            None,
            Seq.empty,
            remoteNetworkLevel = true,
            0,
            0,
            0
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
          blockHeights,
          mempool,
          headerToBodyValidation,
          defaultTransactionSyntaxValidation,
          newPeerCreationAlgebra,
          defaultP2PConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          defaultWarmToHotSelector,
          initialPeersMap,
          defaultCache()
        )
        .use { actor =>
          for {
            _ <- actor.send(PeersManager.Message.GetNetworkQualityForWarmHosts)
          } yield ()
        }
    }
  }

  test("Track common ancestor shall be forwarded to all connections") {
    withMock {
      val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]
      val localChain: LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]]
      val slotDataStore: Store[F, BlockId, SlotData] = mock[Store[F, BlockId, SlotData]]
      val transactionStore: Store[F, TransactionId, IoTransaction] = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree: ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val mempool = mock[MempoolAlgebra[F]]

      val coldHost = arbitraryHost.arbitrary.first
      val coldPeer = mockPeerActor[F]()
      (coldPeer.sendNoWait _)
        .expects(PeerActor.Message.PrintCommonAncestor)
        .returns(().pure[F])

      val warmHost = arbitraryHost.arbitrary.first
      val warmPeer = mockPeerActor[F]()
      (warmPeer.sendNoWait _)
        .expects(PeerActor.Message.PrintCommonAncestor)
        .returns(().pure[F])

      val hotHost = arbitraryHost.arbitrary.first

      val banedHost = arbitraryHost.arbitrary.first

      val initialPeersMap =
        Map[HostId, Peer[F]](
          coldHost -> Peer(
            PeerState.Cold,
            Option(coldPeer),
            coldHost,
            None,
            Seq.empty,
            remoteNetworkLevel = true,
            0,
            0,
            0
          ),
          warmHost -> Peer(
            PeerState.Warm,
            Option(warmPeer),
            warmHost,
            None,
            Seq.empty,
            remoteNetworkLevel = true,
            0,
            0,
            0
          ),
          hotHost -> Peer(PeerState.Hot, None, hotHost, None, Seq.empty, remoteNetworkLevel = true, 0, 0, 0),
          banedHost -> Peer(
            PeerState.Banned,
            None,
            banedHost,
            None,
            Seq.empty,
            remoteNetworkLevel = true,
            0,
            0,
            0
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
          blockHeights,
          mempool,
          headerToBodyValidation,
          defaultTransactionSyntaxValidation,
          newPeerCreationAlgebra,
          defaultP2PConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          defaultWarmToHotSelector,
          initialPeersMap,
          defaultCache()
        )
        .use { actor =>
          for {
            _ <- actor.send(PeersManager.Message.PrintCommonAncestor)
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
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val mempool = mock[MempoolAlgebra[F]]

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
          blockHeights,
          mempool,
          headerToBodyValidation,
          defaultTransactionSyntaxValidation,
          newPeerCreationAlgebra,
          defaultP2PConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          defaultWarmToHotSelector,
          Map.empty,
          defaultCache()
        )
        .use { actor =>
          for {
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
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val mempool = mock[MempoolAlgebra[F]]

      val host1 = "1"
      val peerActor1 = mockPeerActor[F]()
      (peerActor1.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = false, applicationLevel = false))
        .returns(().pure[F])

      val host2 = "2"
      val peerActor2 = mockPeerActor[F]()

      val initialPeersMap =
        Map(
          host1 -> Peer(PeerState.Hot, Option(peerActor1), host1, None, Seq.empty, remoteNetworkLevel = true, 0, 0, 0),
          host2 -> Peer(PeerState.Cold, Option(peerActor2), host2, None, Seq.empty, remoteNetworkLevel = false, 0, 0, 0)
        )

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          blockHeights,
          mempool,
          headerToBodyValidation,
          defaultTransactionSyntaxValidation,
          newPeerCreationAlgebra,
          defaultP2PConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          defaultWarmToHotSelector,
          initialPeersMap,
          defaultCache()
        )
        .use { actor =>
          for {
            preTimestamp <- System.currentTimeMillis().pure[F]
            endState1    <- actor.send(PeersManager.Message.BanPeer(host1))
            _ = assert(endState1.peersHandler(host1).state == PeerState.Banned)
            _ = assert(endState1.peersHandler(host1).closedTimestamps.size == 1)
            timestamp = endState1.peersHandler(host1).closedTimestamps.head
            _ = assert(timestamp >= preTimestamp)
            _ = assert(timestamp <= System.currentTimeMillis())
            endState2 <- actor.send(PeersManager.Message.BanPeer(host2))
            _ = assert(endState2.peersHandler(host2).state == PeerState.Banned)
            _ = assert(endState2.peersHandler(host2).closedTimestamps.isEmpty)
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
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val mempool = mock[MempoolAlgebra[F]]

      val peerActor = mockPeerActor[F]()
      (peerActor.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = false, applicationLevel = false))
        .returns(().pure[F])

      val initialPeersMap =
        Map.empty[HostId, Peer[F]] + (hostId -> Peer(
          PeerState.Hot,
          Option(peerActor),
          hostId,
          None,
          Seq(System.currentTimeMillis()),
          remoteNetworkLevel = true,
          0,
          0,
          0
        ))

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          blockHeights,
          mempool,
          headerToBodyValidation,
          defaultTransactionSyntaxValidation,
          newPeerCreationAlgebra,
          defaultP2PConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          defaultWarmToHotSelector,
          initialPeersMap,
          defaultCache()
        )
        .use { actor =>
          for {
            preTimestamp <- System.currentTimeMillis().pure[F]
            endState     <- actor.send(PeersManager.Message.ClosePeer(hostId))
            _ = assert(endState.peersHandler(hostId).actorOpt.isEmpty)
            _ = assert(endState.peersHandler(hostId).state == PeerState.Cold)
            _ = assert(endState.peersHandler(hostId).closedTimestamps.size == 2)
            timestamp = endState.peersHandler(hostId).closedTimestamps.last
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
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val mempool = mock[MempoolAlgebra[F]]

      val host1 = arbitraryHost.arbitrary.first
      val peerActor1 = mockPeerActor[F]()
      (peerActor1.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = false, applicationLevel = false))
        .returns(().pure[F])

      val host2 = arbitraryHost.arbitrary.first
      val peerActor2 = mockPeerActor[F]()
      (peerActor2.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = false, applicationLevel = false))
        .returns(().pure[F])

      val initialPeersMap =
        Map(
          host1 -> Peer(
            PeerState.Hot,
            Option(peerActor1),
            host1,
            None,
            Seq(System.currentTimeMillis()),
            remoteNetworkLevel = false,
            0,
            0,
            0
          ),
          host2 -> Peer(
            PeerState.Warm,
            Option(peerActor2),
            host2,
            None,
            Seq(System.currentTimeMillis()),
            remoteNetworkLevel = true,
            0,
            0,
            0
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
          blockHeights,
          mempool,
          headerToBodyValidation,
          defaultTransactionSyntaxValidation,
          newPeerCreationAlgebra,
          defaultP2PConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          defaultWarmToHotSelector,
          initialPeersMap,
          defaultCache()
        )
        .use { actor =>
          for {
            preTimestamp <- System.currentTimeMillis().pure[F]
            endState     <- actor.send(PeersManager.Message.MoveToCold(NonEmptyChain(host1, host2)))
            endTimestamp = System.currentTimeMillis()
            _ = assert(endState.peersHandler(host1).actorOpt.isEmpty)
            _ = assert(endState.peersHandler(host1).state == PeerState.Cold)
            _ = assert(endState.peersHandler(host1).closedTimestamps.size == 2)
            timestamp1 = endState.peersHandler(host1).closedTimestamps.last
            _ = assert(timestamp1 >= preTimestamp)
            _ = assert(timestamp1 <= endTimestamp)

            _ = assert(endState.peersHandler(host2).actorOpt.isDefined)
            _ = assert(endState.peersHandler(host2).state == PeerState.Cold)
            _ = assert(endState.peersHandler(host2).closedTimestamps.size == 2)
            timestamp2 = endState.peersHandler(host2).closedTimestamps.last
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
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val mempool = mock[MempoolAlgebra[F]]

      val peerActor = mockPeerActor[F]()
      (peerActor.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = false, applicationLevel = false))
        .returns(().pure[F])

      val timeoutWindows = 1000
      val initialPeersMap =
        Map.empty[HostId, Peer[F]] + (hostId -> Peer(
          PeerState.Hot,
          Option(peerActor),
          hostId,
          None,
          Seq(0, 200, System.currentTimeMillis() - timeoutWindows - 1, System.currentTimeMillis()),
          remoteNetworkLevel = true,
          0,
          0,
          0
        ))

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          blockHeights,
          mempool,
          headerToBodyValidation,
          defaultTransactionSyntaxValidation,
          newPeerCreationAlgebra,
          defaultP2PConfig.copy(networkProperties = NetworkProperties(closeTimeoutWindowInMs = timeoutWindows)),
          defaultHotPeerUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          defaultWarmToHotSelector,
          initialPeersMap,
          defaultCache()
        )
        .use { actor =>
          for {
            preTimestamp <- System.currentTimeMillis().pure[F]
            endState     <- actor.send(PeersManager.Message.MoveToCold(NonEmptyChain(hostId)))
            _ = assert(endState.peersHandler(hostId).state == PeerState.Cold)
            _ = assert(endState.peersHandler(hostId).closedTimestamps.size == 2)
            timestamp = endState.peersHandler(hostId).closedTimestamps.last
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
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val mempool = mock[MempoolAlgebra[F]]

      val p2pConfig: P2PNetworkConfig =
        defaultP2PConfig.copy(networkProperties =
          NetworkProperties(maximumWarmConnections = 2, minimumHotConnections = 0)
        )

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
          blockHeights,
          mempool,
          headerToBodyValidation,
          defaultTransactionSyntaxValidation,
          newPeerCreationAlgebra,
          p2pConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          defaultWarmToHotSelector,
          initialPeers = Map.empty[HostId, Peer[F]],
          blockSource = defaultCache()
        )
        .use { actor =>
          val peers = NonEmptyChain(RemotePeer(host1, 0, 0), RemotePeer(host2, 0, 0), RemotePeer(host3, 0, 0))
          for {
            withColdPeer <- actor.send(PeersManager.Message.AddKnownPeers(peers))
            _ = assert(withColdPeer.peersHandler(host1.host).state == PeerState.Cold)
            _ = assert(withColdPeer.peersHandler(host2.host).state == PeerState.Cold)
            _ = assert(withColdPeer.peersHandler(host3.host).state == PeerState.Cold)

            withUpdate <- actor.send(PeersManager.Message.UpdatedReputationTick)
            _ = assert(withUpdate.peersHandler(host1.host).state == PeerState.Warm)
            _ = assert(withUpdate.peersHandler(host2.host).state == PeerState.Warm)
            _ = assert(withUpdate.peersHandler(host3.host).state == PeerState.Cold)
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
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val mempool = mock[MempoolAlgebra[F]]

      val p2pConfig: P2PNetworkConfig =
        defaultP2PConfig.copy(networkProperties =
          NetworkProperties(maximumWarmConnections = 10, minimumHotConnections = 0)
        )

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
          host1.host -> Peer(
            PeerState.Cold,
            None,
            host1.host,
            Option(host1.port),
            Seq.empty,
            remoteNetworkLevel = true,
            0,
            0,
            0
          ),
          host2.host -> Peer(
            PeerState.Cold,
            None,
            host2.host,
            Option(host2.port),
            Seq.empty,
            remoteNetworkLevel = true,
            0,
            0,
            0
          ),
          host3.host -> Peer(PeerState.Cold, None, host3.host, None, Seq.empty, remoteNetworkLevel = true, 0, 0, 0),
          host4.host -> Peer(
            PeerState.Warm,
            None,
            host4.host,
            Option(host4.port),
            Seq.empty,
            remoteNetworkLevel = true,
            0,
            0,
            0
          ),
          host5.host -> Peer(
            PeerState.Hot,
            None,
            host5.host,
            Option(host5.port),
            Seq.empty,
            remoteNetworkLevel = true,
            0,
            0,
            0
          ),
          host6.host -> Peer(
            PeerState.Banned,
            None,
            host6.host,
            Option(host6.port),
            Seq.empty,
            remoteNetworkLevel = true,
            0,
            0,
            0
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
          blockHeights,
          mempool,
          headerToBodyValidation,
          defaultTransactionSyntaxValidation,
          newPeerCreationAlgebra,
          p2pConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          defaultWarmToHotSelector,
          initialPeersMap,
          defaultCache()
        )
        .use { actor =>
          for {

            withUpdate <- actor.send(PeersManager.Message.UpdatedReputationTick)
            _ = assert(withUpdate.peersHandler(host1.host).state == PeerState.Warm)
            _ = assert(withUpdate.peersHandler(host2.host).state == PeerState.Warm)
            _ = assert(withUpdate.peersHandler(host3.host).state == PeerState.Cold)
            _ = assert(withUpdate.peersHandler(host4.host).state == PeerState.Warm)
            _ = assert(withUpdate.peersHandler(host5.host).state == PeerState.Hot)
            _ = assert(withUpdate.peersHandler(host6.host).state == PeerState.Banned)

          } yield ()
        }
    }
  }

  test("Reputation update: Empty reputation update, cold to warm, warm NOT to hot because no actor yet") {
    withMock {
      val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]
      val localChain: LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]]
      val slotDataStore: Store[F, BlockId, SlotData] = mock[Store[F, BlockId, SlotData]]
      val transactionStore: Store[F, TransactionId, IoTransaction] = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree: ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val mempool = mock[MempoolAlgebra[F]]

      val p2pConfig: P2PNetworkConfig =
        defaultP2PConfig.copy(networkProperties =
          NetworkProperties(maximumWarmConnections = 2, minimumHotConnections = 1)
        )

      val host1 = RemoteAddress("1", 1)
      val host2 = RemoteAddress("2", 2)
      val host3 = RemoteAddress("3", 3)

      val initialPeersMap: Map[HostId, Peer[F]] =
        Map(
          host1.host -> Peer(
            PeerState.Cold,
            None,
            host1.host,
            Option(host1.port),
            Seq.empty,
            remoteNetworkLevel = true,
            0.7,
            0.8,
            0
          ),
          host2.host -> Peer(
            PeerState.Cold,
            None,
            host2.host,
            Option(host2.port),
            Seq.empty,
            remoteNetworkLevel = true,
            0.8,
            0.2,
            0
          ),
          host3.host -> Peer(
            PeerState.Cold,
            None,
            host3.host,
            Option(host3.port),
            Seq.empty,
            remoteNetworkLevel = true,
            0.0,
            0.0,
            0
          )
        )

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
          blockHeights,
          mempool,
          headerToBodyValidation,
          defaultTransactionSyntaxValidation,
          newPeerCreationAlgebra,
          p2pConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          defaultWarmToHotSelector,
          initialPeersMap,
          defaultCache()
        )
        .use { actor =>
          for {
            withUpdate <- actor.send(PeersManager.Message.UpdatedReputationTick)
            _ = assert(withUpdate.peersHandler(host1.host).state == PeerState.Warm)
            _ = assert(withUpdate.peersHandler(host2.host).state == PeerState.Warm)
            _ = assert(withUpdate.peersHandler(host3.host).state == PeerState.Cold)
          } yield ()
        }
    }
  }

  test("Aggressive P2P: move some warm peers to hot") {
    withMock {
      val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]
      val localChain: LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]]
      val slotDataStore: Store[F, BlockId, SlotData] = mock[Store[F, BlockId, SlotData]]
      val transactionStore: Store[F, TransactionId, IoTransaction] = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree: ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val mempool = mock[MempoolAlgebra[F]]

      val p2pConfig: P2PNetworkConfig =
        defaultP2PConfig.copy(networkProperties = NetworkProperties(aggressiveP2PCount = 1))

      val host1 = RemoteAddress("1", 1)
      val host2 = RemoteAddress("2", 2)
      val host3 = RemoteAddress("3", 3)

      val peer1 = mockPeerActor[F]()
      (peer1.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = true, applicationLevel = true))
        .returns(().pure[F])

      val peer2 = mockPeerActor[F]()

      val initialPeersMap: Map[HostId, Peer[F]] =
        Map(
          host1.host -> Peer(
            PeerState.Warm,
            peer1.some,
            host1.host,
            None,
            Seq.empty,
            remoteNetworkLevel = true,
            0.1,
            0.0,
            0
          ),
          host2.host -> Peer(
            PeerState.Warm,
            peer2.some,
            host2.host,
            Option(host2.port),
            Seq.empty,
            remoteNetworkLevel = true,
            0.0,
            0.0,
            0
          ),
          host3.host -> Peer(
            PeerState.Warm,
            None,
            host3.host,
            Option(host3.port),
            Seq.empty,
            remoteNetworkLevel = true,
            0.1,
            0.0,
            0
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
          blockHeights,
          mempool,
          headerToBodyValidation,
          defaultTransactionSyntaxValidation,
          newPeerCreationAlgebra,
          p2pConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          defaultWarmToHotSelector,
          initialPeersMap,
          defaultCache()
        )
        .use { actor =>
          for {
            withUpdate <- actor.send(PeersManager.Message.AggressiveP2PUpdate)
            _ = assert(withUpdate.peersHandler(host1.host).state == PeerState.Hot)
            _ = assert(withUpdate.peersHandler(host2.host).state == PeerState.Warm)
            _ = assert(withUpdate.peersHandler(host3.host).state == PeerState.Warm)
            _ = assert(withUpdate.peersHandler.get(host1.host).get.newRep == p2pConfig.remotePeerNoveltyInSlots)
          } yield ()
        }
    }
  }

  test("Reputation update: Update reputation for eligible peers only") {
    withMock {
      val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]
      val localChain: LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]]
      val slotDataStore: Store[F, BlockId, SlotData] = mock[Store[F, BlockId, SlotData]]
      val transactionStore: Store[F, TransactionId, IoTransaction] = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree: ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val mempool = mock[MempoolAlgebra[F]]

      val p2pConfig: P2PNetworkConfig =
        defaultP2PConfig.copy(networkProperties =
          NetworkProperties(maximumWarmConnections = 0, minimumHotConnections = 0)
        )

      val host1 = RemoteAddress("1", 1)
      val host2 = RemoteAddress("2", 2)
      val host3 = RemoteAddress("3", 3)

      val repBlock = 0.8
      val repPerf = 0.9
      val repNovelty = 2
      val initialPeersMap: Map[HostId, Peer[F]] =
        Map(
          host1.host -> Peer(
            PeerState.Cold,
            None,
            host1.host,
            Option(host1.port),
            Seq.empty,
            remoteNetworkLevel = true,
            repBlock,
            repPerf,
            repNovelty
          ),
          host2.host -> Peer(
            PeerState.Warm,
            None,
            host2.host,
            Option(host2.port),
            Seq.empty,
            remoteNetworkLevel = true,
            repBlock,
            repPerf,
            repNovelty
          ),
          host3.host -> Peer(
            PeerState.Hot,
            None,
            host3.host,
            Option(host3.port),
            Seq.empty,
            remoteNetworkLevel = true,
            repBlock,
            repPerf,
            repNovelty
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
          blockHeights,
          mempool,
          headerToBodyValidation,
          defaultTransactionSyntaxValidation,
          newPeerCreationAlgebra,
          p2pConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          defaultWarmToHotSelector,
          initialPeersMap,
          defaultCache()
        )
        .use { actor =>
          for {
            withUpdate <- actor.send(PeersManager.Message.UpdatedReputationTick)
            peers = withUpdate.peersHandler.peers
            _ = assert(withUpdate.peersHandler(host1.host).state == PeerState.Cold)
            _ = assert(withUpdate.peersHandler(host2.host).state == PeerState.Warm)
            _ = assert(withUpdate.peersHandler(host3.host).state == PeerState.Hot)
            _ = assert(peers(host1.host).perfRep == repPerf)
            _ = assert(peers(host1.host).blockRep == repBlock)
            _ = assert(peers(host1.host).newRep == repNovelty)
            _ = assert(peers(host2.host).perfRep == repPerf)
            _ = assert(peers(host2.host).blockRep == repBlock)
            _ = assert(peers(host2.host).newRep == repNovelty)
            _ = assert(peers(host3.host).perfRep == repPerf)
            _ = assert(peers(host3.host).blockRep == repBlock * defaultP2PConfig.blockNoveltyDecoy)
            _ = assert(peers(host3.host).newRep == repNovelty - 1)
          } yield ()
        }
    }
  }

  test("Reputation update: If no warm peer then move only eligible by timeout and port cold peer(s) to warm") {
    withMock {
      val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]
      val localChain: LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]]
      val slotDataStore: Store[F, BlockId, SlotData] = mock[Store[F, BlockId, SlotData]]
      val transactionStore: Store[F, TransactionId, IoTransaction] = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree: ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val mempool = mock[MempoolAlgebra[F]]
      val p2pConfig: P2PNetworkConfig =
        defaultP2PConfig.copy(networkProperties =
          NetworkProperties(maximumWarmConnections = 5, minimumHotConnections = 0)
        )

      val host1 = RemoteAddress("1", 1)
      val host2 = RemoteAddress("2", 2)
      val host3 = RemoteAddress("3", 3)
      val host4 = RemoteAddress("4", 4)
      val host5 = RemoteAddress("5", 5)
      val host6 = RemoteAddress("6", 6)
      val host7 = RemoteAddress("7", 7)

      (newPeerCreationAlgebra.requestNewPeerCreation _).expects(host1).returns(().pure[F])
      (newPeerCreationAlgebra.requestNewPeerCreation _).expects(host2).returns(().pure[F])
      (newPeerCreationAlgebra.requestNewPeerCreation _).expects(host3).returns(().pure[F])

      val closeTimeoutFirstDelayInMs = p2pConfig.networkProperties.closeTimeoutFirstDelayInMs

      val initialPeersMap: Map[HostId, Peer[F]] =
        Map(
          host1.host -> Peer(
            PeerState.Cold,
            None,
            host1.host,
            Option(host1.port),
            Seq.empty,
            remoteNetworkLevel = true,
            0,
            0,
            0
          ),
          host2.host -> Peer(
            PeerState.Cold,
            None,
            host2.host,
            Option(host2.port),
            Seq(System.currentTimeMillis() - closeTimeoutFirstDelayInMs),
            remoteNetworkLevel = true,
            0,
            0,
            0
          ),
          host3.host -> Peer(
            PeerState.Cold,
            None,
            host3.host,
            Option(host3.port),
            Seq(0, System.currentTimeMillis() - (4 * closeTimeoutFirstDelayInMs)),
            remoteNetworkLevel = true,
            0,
            0,
            0
          ),
          host4.host -> Peer(
            PeerState.Cold,
            None,
            host4.host,
            Option(host4.port),
            Seq(System.currentTimeMillis()),
            remoteNetworkLevel = true,
            0,
            0,
            0
          ),
          host5.host -> Peer(
            PeerState.Cold,
            None,
            host5.host,
            Option(host5.port),
            Seq(0, System.currentTimeMillis() - closeTimeoutFirstDelayInMs),
            remoteNetworkLevel = true,
            0,
            0,
            0
          ),
          host6.host -> Peer(
            PeerState.Cold,
            None,
            host6.host,
            Option(host6.port),
            Seq(0, 1, System.currentTimeMillis() - (4 * closeTimeoutFirstDelayInMs)),
            remoteNetworkLevel = true,
            0,
            0,
            0
          ),
          host7.host -> Peer(
            PeerState.Cold,
            None,
            host7.host,
            None,
            Seq.empty,
            remoteNetworkLevel = true,
            1,
            1,
            1
          )
        )

      val selector = new SelectorColdToWarm[F] {
        override def select(hosts: Map[HostId, Peer[F]], countToReceive: Int): Set[HostId] =
          hosts.keys.take(countToReceive).toSet
      }

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          blockHeights,
          mempool,
          headerToBodyValidation,
          defaultTransactionSyntaxValidation,
          newPeerCreationAlgebra,
          p2pConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          selector,
          defaultWarmToHotSelector,
          initialPeersMap,
          defaultCache()
        )
        .use { actor =>
          for {

            withUpdate <- actor.send(PeersManager.Message.UpdatedReputationTick)
            _ = assert(withUpdate.peersHandler(host1.host).state == PeerState.Warm)
            _ = assert(withUpdate.peersHandler(host2.host).state == PeerState.Warm)
            _ = assert(withUpdate.peersHandler(host3.host).state == PeerState.Warm)
            _ = assert(withUpdate.peersHandler(host4.host).state == PeerState.Cold)
            _ = assert(withUpdate.peersHandler(host5.host).state == PeerState.Cold)
            _ = assert(withUpdate.peersHandler(host6.host).state == PeerState.Cold)
            _ = assert(withUpdate.peersHandler(host7.host).state == PeerState.Cold)
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
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val mempool = mock[MempoolAlgebra[F]]
      val p2pConfig: P2PNetworkConfig =
        defaultP2PConfig.copy(networkProperties = NetworkProperties(maximumWarmConnections = 2))

      val host1 = RemoteAddress("1", 1)
      val host2 = RemoteAddress("2", 2)
      val host3 = RemoteAddress("3", 3)
      val host4 = RemoteAddress("4", 4)

      val initialPeersMap: Map[HostId, Peer[F]] =
        Map(
          host1.host -> Peer(PeerState.Hot, None, host1.host, Option(1), Seq.empty, remoteNetworkLevel = true, 0, 0, 0),
          host2.host -> Peer(PeerState.Banned, None, host2.host, None, Seq.empty, remoteNetworkLevel = true, 0, 0, 0),
          host3.host -> Peer(PeerState.Cold, None, host3.host, Option(3), Seq.empty, remoteNetworkLevel = true, 0, 0, 0)
        )

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          blockHeights,
          mempool,
          headerToBodyValidation,
          defaultTransactionSyntaxValidation,
          newPeerCreationAlgebra,
          p2pConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          defaultWarmToHotSelector,
          initialPeersMap,
          defaultCache()
        )
        .use { actor =>
          val peers = NonEmptyChain(
            RemotePeer(host1, 0, 0),
            RemotePeer(host2, 0, 0),
            RemotePeer(host3, 0, 0),
            RemotePeer(host4, 0, 0)
          )

          for {
            withColdPeer <- actor.send(PeersManager.Message.AddKnownPeers(peers))
            _ = assert(withColdPeer.peersHandler(host1.host).state == PeerState.Hot)
            _ = assert(withColdPeer.peersHandler(host2.host).state == PeerState.Banned)
            _ = assert(withColdPeer.peersHandler(host3.host).state == PeerState.Cold)
            _ = assert(withColdPeer.peersHandler(host4.host).state == PeerState.Cold)
            _ = assert(withColdPeer.peersHandler(host4.host).closedTimestamps == Seq.empty)

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
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val mempool = mock[MempoolAlgebra[F]]

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

      val host6 = RemoteAddress("six", 6)
      val peer6 = mockPeerActor[F]()
      (peer6.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = false, applicationLevel = false))
        .returns(().pure[F])

      val initialPeersMap: Map[HostId, Peer[F]] =
        Map(
          host1.host -> Peer(
            PeerState.Hot,
            Option(peer1),
            host1.host,
            Option(1),
            Seq(1),
            remoteNetworkLevel = true,
            0,
            1.0,
            0
          ),
          host2.host -> Peer(
            PeerState.Hot,
            Option(peer2),
            host2.host,
            None,
            Seq(2),
            remoteNetworkLevel = true,
            1.0,
            0,
            0
          ),
          host3.host -> Peer(
            PeerState.Hot,
            Option(peer3),
            host3.host,
            Option(3),
            Seq(3),
            remoteNetworkLevel = true,
            defaultP2PConfig.networkProperties.minimumRequiredReputation * 1.05,
            defaultP2PConfig.networkProperties.minimumRequiredReputation * 1.05,
            0
          ),
          host4.host -> Peer(
            PeerState.Hot,
            Option(peer4),
            host4.host,
            None,
            Seq(4),
            remoteNetworkLevel = true,
            0,
            0,
            2
          ),
          host5.host -> Peer(
            PeerState.Hot,
            Option(peer5),
            host5.host,
            None,
            Seq(5),
            remoteNetworkLevel = false,
            0,
            0,
            0
          ),
          host6.host -> Peer(PeerState.Hot, Option(peer6), host6.host, None, Seq(6), remoteNetworkLevel = true, 0, 0, 0)
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
          blockHeights,
          mempool,
          headerToBodyValidation,
          defaultTransactionSyntaxValidation,
          newPeerCreationAlgebra,
          defaultP2PConfig,
          hotUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          defaultWarmToHotSelector,
          initialPeersMap,
          defaultCache()
        )
        .use { actor =>
          for {
            withUpdate <- actor.send(PeersManager.Message.UpdatedReputationTick)
            _ = assert(withUpdate.peersHandler(host1.host).state == PeerState.Hot)
            _ = assert(withUpdate.peersHandler(host1.host).closedTimestamps == Seq(1))
            _ = assert(withUpdate.peersHandler(host1.host).actorOpt.isDefined)
            _ = assert(withUpdate.peersHandler(host2.host).state == PeerState.Hot)
            _ = assert(withUpdate.peersHandler(host2.host).closedTimestamps == Seq(2))
            _ = assert(withUpdate.peersHandler(host2.host).actorOpt.isDefined)
            _ = assert(withUpdate.peersHandler(host3.host).state == PeerState.Hot)
            _ = assert(withUpdate.peersHandler(host3.host).closedTimestamps == Seq(3))
            _ = assert(withUpdate.peersHandler(host3.host).actorOpt.isDefined)
            _ = assert(withUpdate.peersHandler(host4.host).state == PeerState.Hot)
            _ = assert(withUpdate.peersHandler(host4.host).closedTimestamps == Seq(4))
            _ = assert(withUpdate.peersHandler(host4.host).actorOpt.isDefined)
            _ = assert(withUpdate.peersHandler(host5.host).state == PeerState.Cold)
            _ = assert(withUpdate.peersHandler(host5.host).closedTimestamps.size == 2)
            _ = assert(withUpdate.peersHandler(host5.host).actorOpt.isEmpty)
            _ = assert(withUpdate.peersHandler(host6.host).state == PeerState.Cold)
            _ = assert(withUpdate.peersHandler(host6.host).closedTimestamps.size == 2)
            _ = assert(withUpdate.peersHandler(host6.host).actorOpt.isDefined)
          } yield ()
        }
    }
  }

  test("Fully closed peer and then open it again shall create new peer actor") {
    withMock {
      val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]
      val localChain: LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]]
      val slotDataStore: Store[F, BlockId, SlotData] = mock[Store[F, BlockId, SlotData]]
      val transactionStore: Store[F, TransactionId, IoTransaction] = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree: ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val requestProxy = mock[RequestsProxyActor[F]]
      val mempool = mock[MempoolAlgebra[F]]

      val client1 = mock[BlockchainPeerClient[F]]
      val client2 = mock[BlockchainPeerClient[F]]
      val host1 = RemoteAddress("first", 1)
      val peer1 = mockPeerActor[F]()
      val peer2 = mockPeerActor[F]()

      (networkAlgebra.makePeer _)
        .expects(host1.host, *, *, *, *, *, *, *, *, *, *, *, *)
        .once()
        .returns(
          Resource
            .pure(peer1)
            .onFinalize(Sync[F].delay(peer1.sendNoWait(PeerActor.Message.CloseConnection)))
        ) // simulate real actor finalizer
      (peer1.sendNoWait _).expects(PeerActor.Message.GetPeerServerAddress).returns(Applicative[F].unit)
      (peer1.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = false, applicationLevel = false))
        .returns(Applicative[F].unit)
      (peer1.sendNoWait _)
        .expects(PeerActor.Message.GetNetworkQuality)
        .returns(Applicative[F].unit)
      (peer1.sendNoWait _).expects(PeerActor.Message.CloseConnection).returns(Applicative[F].unit)

      (networkAlgebra.makePeer _)
        .expects(host1.host, *, *, *, *, *, *, *, *, *, *, *, *)
        .once()
        .returns(Resource.pure(peer2))
      (peer2.sendNoWait _).expects(PeerActor.Message.GetPeerServerAddress).returns(Applicative[F].unit)
      (peer2.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = false, applicationLevel = false))
        .returns(Applicative[F].unit)
      (peer2.sendNoWait _)
        .expects(PeerActor.Message.GetNetworkQuality)
        .returns(Applicative[F].unit)

      val initialPeersMap: Map[HostId, Peer[F]] = Map.empty

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          blockHeights,
          mempool,
          headerToBodyValidation,
          defaultTransactionSyntaxValidation,
          newPeerCreationAlgebra,
          defaultP2PConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          defaultWarmToHotSelector,
          initialPeersMap,
          defaultCache()
        )
        .use { actor =>
          for {
            _          <- actor.send(PeersManager.Message.SetupRequestsProxy(requestProxy))
            withUpdate <- actor.send(PeersManager.Message.OpenedPeerConnection(host1, client1))
            _ = assert(withUpdate.peersHandler(host1.host).state == PeerState.Cold)
            _ = assert(withUpdate.peersHandler(host1.host).actorOpt.get == peer1)
            _ = assert(withUpdate.peersHandler(host1.host).remoteServerPort.isEmpty)
            withUpdate2 <- actor.send(PeersManager.Message.ClosePeer(host1.host))
            _ = assert(withUpdate2.peersHandler(host1.host).state == PeerState.Cold)
            _ = assert(withUpdate2.peersHandler(host1.host).actorOpt.isEmpty)
            withUpdate3 <- actor.send(PeersManager.Message.OpenedPeerConnection(host1, client2))
            _ = assert(withUpdate3.peersHandler(host1.host).state == PeerState.Cold)
            _ = assert(withUpdate3.peersHandler(host1.host).actorOpt.get == peer2)
          } yield ()
        }
    }
  }

  test("Cold peer and then incoming new connection shall create new peer actor") {
    withMock {
      val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]
      val localChain: LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]]
      val slotDataStore: Store[F, BlockId, SlotData] = mock[Store[F, BlockId, SlotData]]
      val transactionStore: Store[F, TransactionId, IoTransaction] = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree: ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val requestProxy = mock[RequestsProxyActor[F]]
      val mempool = mock[MempoolAlgebra[F]]

      val client1 = mock[BlockchainPeerClient[F]]
      val host1 = RemoteAddress("first", 1)
      val peer1 = mockPeerActor[F]()

      (networkAlgebra.makePeer _)
        .expects(host1.host, *, *, *, *, *, *, *, *, *, *, *, *)
        .once()
        .returns(
          Resource
            .pure(peer1)
            .onFinalize(Sync[F].delay(peer1.sendNoWait(PeerActor.Message.CloseConnection)))
        ) // simulate real actor finalizer
      (peer1.sendNoWait _).expects(PeerActor.Message.GetPeerServerAddress).returns(Applicative[F].unit)
      (peer1.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = false, applicationLevel = false))
        .returns(Applicative[F].unit)
      (peer1.sendNoWait _)
        .expects(PeerActor.Message.GetNetworkQuality)
        .returns(Applicative[F].unit)
      (peer1.sendNoWait _).expects(PeerActor.Message.CloseConnection).returns(Applicative[F].unit)

      val initialPeersMap: Map[HostId, Peer[F]] =
        Map(host1.host -> Peer(PeerState.Cold, None, host1.host, None, Seq.empty, remoteNetworkLevel = false, 0, 0, 0))

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          blockHeights,
          mempool,
          headerToBodyValidation,
          defaultTransactionSyntaxValidation,
          newPeerCreationAlgebra,
          defaultP2PConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          defaultWarmToHotSelector,
          initialPeersMap,
          defaultCache()
        )
        .use { actor =>
          for {
            _          <- actor.send(PeersManager.Message.SetupRequestsProxy(requestProxy))
            withUpdate <- actor.send(PeersManager.Message.OpenedPeerConnection(host1, client1))
            _ = assert(withUpdate.peersHandler(host1.host).state == PeerState.Cold)
            _ = assert(withUpdate.peersHandler(host1.host).actorOpt.get == peer1)
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
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val mempool = mock[MempoolAlgebra[F]]
      val p2pConfig: P2PNetworkConfig =
        defaultP2PConfig.copy(networkProperties = NetworkProperties(minimumHotConnections = 2))

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
          host1.host -> Peer(
            PeerState.Warm,
            Option(peer1),
            host1.host,
            None,
            Seq(1),
            remoteNetworkLevel = true,
            0,
            1.0,
            0
          ),
          host2.host -> Peer(
            PeerState.Warm,
            Option(peer2),
            host2.host,
            Option(2),
            Seq(2),
            remoteNetworkLevel = true,
            0,
            0.9,
            0
          ),
          host3.host -> Peer(
            PeerState.Warm,
            Option(peer3),
            host3.host,
            None,
            Seq(3),
            remoteNetworkLevel = true,
            0,
            0.8,
            0
          ),
          host4.host -> Peer(
            PeerState.Warm,
            Option(peer4),
            host4.host,
            None,
            Seq(4),
            remoteNetworkLevel = true,
            0,
            0.7,
            0
          )
        )

      val hotUpdater = mock[Set[RemoteAddress] => F[Unit]]
      (hotUpdater.apply _).expects(Set(host2)).once().returns(().pure[F]) // skip host1, because it have no server port

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          blockHeights,
          mempool,
          headerToBodyValidation,
          defaultTransactionSyntaxValidation,
          newPeerCreationAlgebra,
          p2pConfig,
          hotUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          defaultWarmToHotSelector,
          initialPeersMap,
          defaultCache()
        )
        .use { actor =>
          for {
            withUpdate <- actor.send(PeersManager.Message.UpdatedReputationTick)
            _ = assert(withUpdate.peersHandler(host1.host).state == PeerState.Hot)
            _ = assert(withUpdate.peersHandler(host1.host).closedTimestamps == Seq(1))
            _ = assert(withUpdate.peersHandler(host1.host).newRep == defaultP2PConfig.remotePeerNoveltyInSlots)
            _ = assert(withUpdate.peersHandler(host2.host).state == PeerState.Hot)
            _ = assert(withUpdate.peersHandler(host2.host).closedTimestamps == Seq(2))
            _ = assert(withUpdate.peersHandler(host2.host).newRep == defaultP2PConfig.remotePeerNoveltyInSlots)
            _ = assert(withUpdate.peersHandler(host3.host).state == PeerState.Warm)
            _ = assert(withUpdate.peersHandler(host3.host).closedTimestamps == Seq(3))
            _ = assert(withUpdate.peersHandler(host4.host).state == PeerState.Warm)
            _ = assert(withUpdate.peersHandler(host4.host).closedTimestamps == Seq(4))
          } yield ()
        }
    }
  }

  test("Peer processing after received new opened peer message") {
    withMock {

      val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]
      val localChain: LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]]
      val slotDataStore: Store[F, BlockId, SlotData] = mock[Store[F, BlockId, SlotData]]
      val transactionStore: Store[F, TransactionId, IoTransaction] = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree: ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val blockChecker = mock[BlockCheckerActor[F]]
      val requestProxy = mock[RequestsProxyActor[F]]
      val mempool = mock[MempoolAlgebra[F]]

      val host1 = RemoteAddress("1", 1)
      val peer1 = mockPeerActor[F]()
      (networkAlgebra.makePeer _)
        .expects(host1.host, *, *, *, *, *, *, *, *, *, *, *, *)
        .once()
        .returns(Resource.pure(peer1))
      (peer1.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = false, applicationLevel = false))
        .returns(Applicative[F].unit)
      (peer1.sendNoWait _)
        .expects(PeerActor.Message.GetPeerServerAddress)
        .returns(Applicative[F].unit)
      (peer1.sendNoWait _)
        .expects(PeerActor.Message.GetNetworkQuality)
        .returns(Applicative[F].unit)

      val host2 = RemoteAddress("2", 2)
      val peer2 = mockPeerActor[F]()
      (networkAlgebra.makePeer _)
        .expects(host2.host, *, *, *, *, *, *, *, *, *, *, *, *)
        .once()
        .returns(Resource.pure(peer2))
      (peer2.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = false, applicationLevel = false))
        .returns(Applicative[F].unit)
      (peer2.sendNoWait _)
        .expects(PeerActor.Message.GetPeerServerAddress)
        .returns(Applicative[F].unit)
      (peer2.sendNoWait _)
        .expects(PeerActor.Message.GetNetworkQuality)
        .returns(Applicative[F].unit)

      val host3 = RemoteAddress("3", 3)
      val peer3 = mockPeerActor[F]()
      (networkAlgebra.makePeer _)
        .expects(host3.host, *, *, *, *, *, *, *, *, *, *, *, *)
        .once()
        .returns(Resource.pure(peer3))
      (peer3.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = true, applicationLevel = false))
        .returns(Applicative[F].unit)
      (peer3.sendNoWait _)
        .expects(PeerActor.Message.GetPeerServerAddress)
        .returns(Applicative[F].unit)
      (peer3.sendNoWait _)
        .expects(PeerActor.Message.GetNetworkQuality)
        .returns(Applicative[F].unit)

      val host4 = RemoteAddress("4", 4)
      val client4 = mock[BlockchainPeerClient[F]]
      (client4.closeConnection _).expects().once().returns(().pure[F])

      val host5 = RemoteAddress("5", 5)
      val peer5 = mockPeerActor[F]()
      (networkAlgebra.makePeer _)
        .expects(host5.host, *, *, *, *, *, *, *, *, *, *, *, *)
        .once()
        .returns(Resource.pure(peer5))
      (peer5.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = true, applicationLevel = true))
        .returns(Applicative[F].unit)
      (peer5.sendNoWait _)
        .expects(PeerActor.Message.GetPeerServerAddress)
        .returns(Applicative[F].unit)
      (peer5.sendNoWait _)
        .expects(PeerActor.Message.GetNetworkQuality)
        .returns(Applicative[F].unit)

      val host6 = RemoteAddress("6", 6)
      val peer6 = mockPeerActor[F]()

      val host7 = RemoteAddress("7", 7)
      val peer7 = mockPeerActor[F]()

      val initialPeersMap = Map(
        host1.host -> Peer(PeerState.Cold, None, host1.host, None, Seq(1), remoteNetworkLevel = false, 0, 0, 0),
        host3.host -> Peer(PeerState.Warm, None, host3.host, None, Seq(3), remoteNetworkLevel = true, 0, 0, 0),
        host4.host -> Peer(PeerState.Banned, None, host4.host, None, Seq(4), remoteNetworkLevel = true, 0, 0, 0),
        host5.host -> Peer(PeerState.Hot, None, host5.host, None, Seq(5), remoteNetworkLevel = true, 0, 0, 0),
        host6.host -> Peer(PeerState.Cold, Option(peer6), host6.host, None, Seq(6), remoteNetworkLevel = true, 0, 0, 0),
        host7.host -> Peer(PeerState.Warm, Option(peer7), host7.host, None, Seq(7), remoteNetworkLevel = false, 0, 0, 0)
      )

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          blockHeights,
          mempool,
          headerToBodyValidation,
          defaultTransactionSyntaxValidation,
          newPeerCreationAlgebra,
          defaultP2PConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          defaultWarmToHotSelector,
          initialPeersMap,
          defaultCache()
        )
        .use { actor =>
          for {
            _          <- actor.send(PeersManager.Message.SetupBlockChecker(blockChecker))
            _          <- actor.send(PeersManager.Message.SetupRequestsProxy(requestProxy))
            stateHost1 <- actor.send(PeersManager.Message.OpenedPeerConnection(host1, mock[BlockchainPeerClient[F]]))
            _ = assert(stateHost1.peersHandler(host1.host).state == PeerState.Cold)
            _ = assert(stateHost1.peersHandler(host1.host).closedTimestamps == Seq(1))
            _ = assert(stateHost1.peersHandler(host1.host).remoteServerPort.isEmpty)
            stateHost2 <- actor.send(PeersManager.Message.OpenedPeerConnection(host2, mock[BlockchainPeerClient[F]]))
            _ = assert(stateHost2.peersHandler(host2.host).state == PeerState.Cold)
            _ = assert(stateHost2.peersHandler(host2.host).closedTimestamps == Seq.empty)
            _ = assert(stateHost2.peersHandler(host2.host).remoteServerPort.isEmpty)
            stateHost3 <- actor.send(PeersManager.Message.OpenedPeerConnection(host3, mock[BlockchainPeerClient[F]]))
            _ = assert(stateHost3.peersHandler(host3.host).state == PeerState.Warm)
            _ = assert(stateHost3.peersHandler(host3.host).closedTimestamps == Seq(3))
            _ = assert(stateHost3.peersHandler(host3.host).remoteServerPort.isEmpty)
            stateHost4 <- actor.send(PeersManager.Message.OpenedPeerConnection(host4, client4))
            _ = assert(stateHost4.peersHandler(host4.host).state == PeerState.Banned)
            _ = assert(stateHost4.peersHandler(host4.host).closedTimestamps == Seq(4))
            _ = assert(stateHost4.peersHandler(host4.host).remoteServerPort.isEmpty)
            stateHost5 <- actor.send(PeersManager.Message.OpenedPeerConnection(host5, mock[BlockchainPeerClient[F]]))
            _ = assert(stateHost5.peersHandler(host5.host).state == PeerState.Hot)
            _ = assert(stateHost5.peersHandler(host5.host).closedTimestamps == Seq(5))
            _ = assert(stateHost5.peersHandler(host5.host).remoteServerPort.isEmpty)
            stateHost6 <- actor.send(PeersManager.Message.OpenedPeerConnection(host6, mock[BlockchainPeerClient[F]]))
            _ = assert(stateHost6.peersHandler(host6.host).state == PeerState.Cold)
            _ = assert(stateHost6.peersHandler(host6.host).closedTimestamps == Seq(6))
            _ = assert(stateHost6.peersHandler(host6.host).remoteServerPort.isEmpty)
            stateHost7 <- actor.send(PeersManager.Message.OpenedPeerConnection(host7, mock[BlockchainPeerClient[F]]))
            _ = assert(stateHost7.peersHandler(host7.host).state == PeerState.Warm)
            _ = assert(stateHost7.peersHandler(host7.host).closedTimestamps == Seq(7))
            _ = assert(stateHost7.peersHandler(host7.host).remoteServerPort.isEmpty)
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
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val blockChecker = mock[BlockCheckerActor[F]]
      val requestProxy = mock[RequestsProxyActor[F]]
      val mempool = mock[MempoolAlgebra[F]]

      val host1 = RemoteAddress("1", 1)
      val peer1 = mockPeerActor[F]()

      val host2 = RemoteAddress("2", 2)
      val peer2 = mockPeerActor[F]()

      val host3 = RemoteAddress("3", 3)
      val peer3 = mockPeerActor[F]()

      val host4 = RemoteAddress("4", 4)
      val peer4 = mockPeerActor[F]()

      val initialPeersMap = Map(
        host1.host -> Peer(
          PeerState.Banned,
          Option(peer1),
          host1.host,
          None,
          Seq(1),
          remoteNetworkLevel = true,
          0,
          0,
          0
        ),
        host2.host -> Peer(PeerState.Cold, Option(peer2), host2.host, None, Seq(3), remoteNetworkLevel = true, 0, 0, 0),
        host3.host -> Peer(PeerState.Warm, Option(peer3), host3.host, None, Seq(4), remoteNetworkLevel = true, 0, 0, 0),
        host4.host -> Peer(PeerState.Hot, Option(peer4), host4.host, None, Seq(5), remoteNetworkLevel = true, 0, 0, 0)
      )

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          blockHeights,
          mempool,
          headerToBodyValidation,
          defaultTransactionSyntaxValidation,
          newPeerCreationAlgebra,
          defaultP2PConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          defaultWarmToHotSelector,
          initialPeersMap,
          defaultCache()
        )
        .use { actor =>
          for {
            _          <- actor.send(PeersManager.Message.SetupBlockChecker(blockChecker))
            _          <- actor.send(PeersManager.Message.SetupRequestsProxy(requestProxy))
            stateHost1 <- actor.send(PeersManager.Message.RemotePeerNetworkLevel(host1.host, networkLevel = false))
            _ = assert(stateHost1.peersHandler(host1.host).state == PeerState.Banned)
            _ = assert(!stateHost1.peersHandler(host1.host).remoteNetworkLevel)
            _ = assert(stateHost1.peersHandler(host1.host).actorOpt.isEmpty)
            stateHost2 <- actor.send(PeersManager.Message.RemotePeerNetworkLevel(host2.host, networkLevel = false))
            _ = assert(stateHost2.peersHandler(host2.host).state == PeerState.Cold)
            _ = assert(!stateHost2.peersHandler(host2.host).remoteNetworkLevel)
            _ = assert(stateHost2.peersHandler(host2.host).actorOpt.isEmpty)
            stateHost3 <- actor.send(PeersManager.Message.RemotePeerNetworkLevel(host3.host, networkLevel = false))
            _ = assert(stateHost3.peersHandler(host3.host).state == PeerState.Warm)
            _ = assert(!stateHost3.peersHandler(host3.host).remoteNetworkLevel)
            _ = assert(stateHost3.peersHandler(host3.host).actorOpt.isDefined)
            stateHost4 <- actor.send(PeersManager.Message.RemotePeerNetworkLevel(host4.host, networkLevel = false))
            _ = assert(stateHost4.peersHandler(host4.host).state == PeerState.Hot)
            _ = assert(!stateHost4.peersHandler(host4.host).remoteNetworkLevel)
            _ = assert(stateHost4.peersHandler(host4.host).actorOpt.isDefined)
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
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val blockChecker = mock[BlockCheckerActor[F]]
      val requestProxy = mock[RequestsProxyActor[F]]
      val mempool = mock[MempoolAlgebra[F]]

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
          host1.host -> Peer(
            PeerState.Hot,
            Option(peer1),
            host1.host,
            None,
            Seq(1),
            remoteNetworkLevel = true,
            0,
            0,
            0
          ),
          host3.host -> Peer(
            PeerState.Hot,
            Option(peer3),
            host3.host,
            None,
            Seq(3),
            remoteNetworkLevel = true,
            0,
            0,
            0
          ),
          host4.host -> Peer(
            PeerState.Hot,
            Option(peer4),
            host4.host,
            Option(host4serverAddress.port),
            Seq(4),
            remoteNetworkLevel = true,
            0,
            0,
            0
          ),
          host5.host -> Peer(
            PeerState.Warm,
            Option(peer5),
            host5.host,
            Option(hostServer5.port),
            Seq(5),
            remoteNetworkLevel = true,
            0,
            0,
            0
          )
        )

      val hotUpdater = mock[Set[RemoteAddress] => F[Unit]]
      (hotUpdater.apply _)
        .expects(
          Set(
            host1serverAddress.copy(host = host1serverAddress.host.reverse),
            host4serverAddress.copy(host = host4serverAddress.host.reverse)
          )
        )
        .twice()
        .returns(().pure[F])

      implicit val dummyReverseReverseDns: ReverseDnsResolver[F] = (h: HostId) => h.reverse.pure[F]
      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          blockHeights,
          mempool,
          headerToBodyValidation,
          defaultTransactionSyntaxValidation,
          newPeerCreationAlgebra,
          defaultP2PConfig,
          hotUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          defaultWarmToHotSelector,
          initialPeersMap,
          defaultCache()
        )(implicitly[Async[IO]], logger, dummyDns, dummyReverseReverseDns)
        .use { actor =>
          for {
            _         <- actor.send(PeersManager.Message.SetupBlockChecker(blockChecker))
            _         <- actor.send(PeersManager.Message.SetupRequestsProxy(requestProxy))
            newState1 <- actor.send(PeersManager.Message.RemotePeerServerPort(host1.host, hostServer1Port))
            _ = assert(newState1.peersHandler(host1.host).remoteServerPort == Option(host1serverAddress.port))
            _ = assert(newState1.peersHandler(host1.host).closedTimestamps == Seq(1))
            newState2 <- actor.send(PeersManager.Message.RemotePeerServerPort(host2.host, hostServer2.port))
            _ = assert(newState1.peersHandler == newState2.peersHandler)
            _ = assert(newState2.peersHandler(host5.host).closedTimestamps == Seq(5))
          } yield ()
        }
    }
  }

  test("Finalized actor shall write non-banned hosts by using their hostnames") {
    withMock {
      val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]
      val localChain: LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]]
      val slotDataStore: Store[F, BlockId, SlotData] = mock[Store[F, BlockId, SlotData]]
      val transactionStore: Store[F, TransactionId, IoTransaction] = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree: ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val blockChecker = mock[BlockCheckerActor[F]]
      val requestProxy = mock[RequestsProxyActor[F]]
      val mempool = mock[MempoolAlgebra[F]]

      val host1 = "host1"
      val hostServer1Port = 1

      val host2 = "host2"
      val hostServer2Port = 2

      val host3 = "host3"
      val hostServer3Port = 3

      val host4 = "host4"
      val hostServer4Port = 4

      val host5 = "host5"
      val hostServer5Port = 5

      val host6 = "host6"

      val initialPeersMap: Map[HostId, Peer[F]] =
        Map(
          host1 -> Peer(
            PeerState.Banned,
            None,
            host1,
            Option(hostServer1Port),
            Seq.empty,
            remoteNetworkLevel = true,
            0,
            0,
            0
          ),
          host2 -> Peer(
            PeerState.Cold,
            None,
            host2,
            Option(hostServer2Port),
            Seq.empty,
            remoteNetworkLevel = true,
            0,
            0,
            0
          ),
          host3 -> Peer(
            PeerState.Warm,
            None,
            host3,
            Option(hostServer3Port),
            Seq.empty,
            remoteNetworkLevel = true,
            0,
            0,
            0
          ),
          host4 -> Peer(
            PeerState.Warm,
            None,
            host4,
            Option(hostServer4Port),
            Seq.empty,
            remoteNetworkLevel = true,
            0,
            0,
            0
          ),
          host5 -> Peer(
            PeerState.Hot,
            None,
            host5,
            Option(hostServer5Port),
            Seq.empty,
            remoteNetworkLevel = true,
            0,
            0,
            0
          ),
          host6 -> Peer(PeerState.Hot, None, host6, None, Seq.empty, remoteNetworkLevel = true, 0, 0, 0)
        )

      val writingHosts = mock[Set[RemotePeer] => F[Unit]]
      val expectedHosts = Set(
        RemotePeer(RemoteAddress(host2.reverse, hostServer2Port), 0, 0),
        RemotePeer(RemoteAddress(host3.reverse, hostServer3Port), 0, 0),
        RemotePeer(RemoteAddress(host4.reverse, hostServer4Port), 0, 0),
        RemotePeer(RemoteAddress(host5.reverse, hostServer5Port), 0, 0)
      )
      (writingHosts.apply _).expects(expectedHosts).once().returns(().pure[F])

      implicit val dummyReverseReverseDns: ReverseDnsResolver[F] = (h: HostId) => h.reverse.pure[F]
      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          blockHeights,
          mempool,
          headerToBodyValidation,
          defaultTransactionSyntaxValidation,
          newPeerCreationAlgebra,
          defaultP2PConfig,
          defaultHotPeerUpdater,
          writingHosts,
          defaultColdToWarmSelector,
          defaultWarmToHotSelector,
          initialPeersMap,
          defaultCache()
        )(implicitly[Async[IO]], logger, dummyDns, dummyReverseReverseDns)
        .use { actor =>
          for {
            _ <- actor.send(PeersManager.Message.SetupBlockChecker(blockChecker))
            _ <- actor.send(PeersManager.Message.SetupRequestsProxy(requestProxy))
          } yield ()
        }
    }
  }

  test("Add known neighbours shall correctly filter out special IP addresses and local address") {
    withMock {
      val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]
      val localChain: LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]]
      val slotDataStore: Store[F, BlockId, SlotData] = mock[Store[F, BlockId, SlotData]]
      val transactionStore: Store[F, TransactionId, IoTransaction] = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree: ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] =
        mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val blockChecker = mock[BlockCheckerActor[F]]
      val requestProxy = mock[RequestsProxyActor[F]]
      val mempool = mock[MempoolAlgebra[F]]

      val someHost = arbitraryHost.arbitrary.first

      val externalIPs = Set("126.0.0.0", "8.8.8.8")
      val specialIPs =
        Set(
          "0.0.0.0",
          "127.127.127.127",
          "238.255.255.255",
          "224.0.0.0",
          "0.0.0.0",
          "255.255.255.255",
          "wrong ip"
        )

      val loopBack = Set(thisHostId)

      val remoteAddresses =
        NonEmptyChain.fromSeq((externalIPs ++ specialIPs ++ loopBack).map(ip => RemoteAddress(ip, 0)).toSeq).get
      val peersToAdd = remoteAddresses.map(ra => RemotePeer(ra, 0, 0))

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          blockHeights,
          mempool,
          headerToBodyValidation,
          defaultTransactionSyntaxValidation,
          newPeerCreationAlgebra,
          defaultP2PConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          defaultWarmToHotSelector,
          Map.empty,
          defaultCache()
        )
        .use { actor =>
          for {
            _            <- actor.send(PeersManager.Message.SetupBlockChecker(blockChecker))
            _            <- actor.send(PeersManager.Message.SetupRequestsProxy(requestProxy))
            updatedState <- actor.send(PeersManager.Message.AddKnownNeighbors(someHost, remoteAddresses))
            knownPeers1 = updatedState.peersHandler.peers.keySet
            _ = assert(externalIPs.forall(knownPeers1.contains))
            _ = assert(specialIPs.forall(!knownPeers1.contains(_)))
            updateWithAddKnown <- actor.send(PeersManager.Message.AddKnownPeers(peersToAdd))
            knownPeers2 = updateWithAddKnown.peersHandler.peers.keySet
            _ = assert(externalIPs.forall(knownPeers2.contains))
            _ = assert(specialIPs.forall(knownPeers2.contains))
          } yield ()
        }
    }

  }

  test("Add known neighbours shall correctly set last known reputation based on source") {
    withMock {
      val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]
      val localChain: LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]]
      val slotDataStore: Store[F, BlockId, SlotData] = mock[Store[F, BlockId, SlotData]]
      val transactionStore: Store[F, TransactionId, IoTransaction] = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree: ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] =
        mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val blockChecker = mock[BlockCheckerActor[F]]
      val requestProxy = mock[RequestsProxyActor[F]]
      val mempool = mock[MempoolAlgebra[F]]

      val address1 = RemoteAddress("10.0.0.1", 1)
      val address2 = RemoteAddress("10.0.0.2", 2)

      val host1 = "1"
      val hostServer1Port = 1
      val host1BlockRep = 0.6
      val host1PerfRep = 0.8

      val initialPeersMap: Map[HostId, Peer[F]] =
        Map(
          host1 -> Peer(
            PeerState.Hot,
            None,
            host1,
            Option(hostServer1Port),
            Seq.empty,
            remoteNetworkLevel = true,
            host1BlockRep,
            host1PerfRep,
            0
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
          blockHeights,
          mempool,
          headerToBodyValidation,
          defaultTransactionSyntaxValidation,
          newPeerCreationAlgebra,
          defaultP2PConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          defaultWarmToHotSelector,
          initialPeersMap,
          defaultCache()
        )
        .use { actor =>
          for {
            _            <- actor.send(PeersManager.Message.SetupBlockChecker(blockChecker))
            _            <- actor.send(PeersManager.Message.SetupRequestsProxy(requestProxy))
            updatedState <- actor.send(PeersManager.Message.AddKnownNeighbors(host1, NonEmptyChain(address1, address2)))
            knownPeers1 = updatedState.peersHandler.peers
            _ = assert(knownPeers1(address1.host).blockRep == host1BlockRep)
            _ = assert(knownPeers1(address1.host).perfRep == 0.0)
            _ = assert(knownPeers1(address2.host).blockRep == host1BlockRep)
            _ = assert(knownPeers1(address2.host).perfRep == 0.0)
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
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] =
        mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val blockChecker = mock[BlockCheckerActor[F]]
      val requestProxy = mock[RequestsProxyActor[F]]
      val mempool = mock[MempoolAlgebra[F]]

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
        defaultCache()
      initialCash.putAll(blockWithSource.asJava)

      val peer: PeerActor[F] = mockPeerActor[F]()
      val initialPeersMap: Map[HostId, Peer[F]] = Map(
        blockSource -> Peer(
          PeerState.Hot,
          Option(peer),
          blockSource,
          None,
          Seq.empty,
          remoteNetworkLevel = true,
          0,
          0,
          0
        ),
        coldPeer -> Peer(
          PeerState.Cold,
          Option(mockPeerActor[F]()),
          coldPeer,
          None,
          Seq.empty,
          remoteNetworkLevel = true,
          0,
          0,
          0
        ),
        preWarmPeer -> Peer(
          PeerState.Warm,
          Option(mockPeerActor[F]()),
          preWarmPeer,
          None,
          Seq.empty,
          remoteNetworkLevel = true,
          0,
          0,
          0
        ),
        bannedPeer -> Peer(
          PeerState.Banned,
          Option(mockPeerActor[F]()),
          bannedPeer,
          None,
          Seq.empty,
          remoteNetworkLevel = true,
          0,
          0,
          0
        ),
        warmPeer -> Peer(
          PeerState.Warm,
          Option(mockPeerActor[F]()),
          warmPeer,
          None,
          Seq.empty,
          remoteNetworkLevel = true,
          0,
          0,
          0
        )
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
          blockHeights,
          mempool,
          headerToBodyValidation,
          defaultTransactionSyntaxValidation,
          newPeerCreationAlgebra,
          defaultP2PConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          defaultWarmToHotSelector,
          initialPeersMap,
          initialCash
        )
        .use { actor =>
          for {
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
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] =
        mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val blockChecker = mock[BlockCheckerActor[F]]
      val requestProxy = mock[RequestsProxyActor[F]]
      val mempool = mock[MempoolAlgebra[F]]

      val blockHeader1 = ModelGenerators.arbitraryHeader.arbitrary.first.embedId
      val blockHeader2 = ModelGenerators.arbitraryHeader.arbitrary.first.embedId

      val blockSource: HostId = arbitraryHost.arbitrary.first

      val blockWithSource: Map[BlockId, Set[HostId]] = Map()

      val initialCash =
        defaultCache()
      initialCash.putAll(blockWithSource.asJava)

      val peer: PeerActor[F] = mockPeerActor[F]()
      val initialPeersMap: Map[HostId, Peer[F]] = Map(
        blockSource -> Peer(
          PeerState.Hot,
          Option(peer),
          blockSource,
          None,
          Seq.empty,
          remoteNetworkLevel = true,
          0,
          0,
          0
        )
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
          blockHeights,
          mempool,
          headerToBodyValidation,
          defaultTransactionSyntaxValidation,
          newPeerCreationAlgebra,
          defaultP2PConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          defaultWarmToHotSelector,
          initialPeersMap,
          initialCash
        )
        .use { actor =>
          for {
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
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] =
        mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val blockChecker = mock[BlockCheckerActor[F]]
      val requestProxy = mock[RequestsProxyActor[F]]
      val mempool = mock[MempoolAlgebra[F]]

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
        defaultCache()
      initialCash.putAll(blockWithSource.asJava)

      val initialPeersMap: Map[HostId, Peer[F]] = Map(
        coldPeer -> Peer(
          PeerState.Cold,
          Option(mockPeerActor[F]()),
          coldPeer,
          None,
          Seq.empty,
          remoteNetworkLevel = true,
          0,
          0,
          0
        ),
        preWarmPeer -> Peer(
          PeerState.Warm,
          Option(mockPeerActor[F]()),
          preWarmPeer,
          None,
          Seq.empty,
          remoteNetworkLevel = true,
          0,
          0,
          0
        ),
        bannedPeer -> Peer(
          PeerState.Banned,
          Option(mockPeerActor[F]()),
          bannedPeer,
          None,
          Seq.empty,
          remoteNetworkLevel = true,
          0,
          0,
          0
        ),
        warmPeer -> Peer(
          PeerState.Warm,
          Option(mockPeerActor[F]()),
          warmPeer,
          None,
          Seq.empty,
          remoteNetworkLevel = true,
          0,
          0,
          0
        )
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
          blockHeights,
          mempool,
          headerToBodyValidation,
          defaultTransactionSyntaxValidation,
          newPeerCreationAlgebra,
          defaultP2PConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          defaultWarmToHotSelector,
          initialPeersMap,
          initialCash
        )
        .use { actor =>
          for {
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
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] =
        mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val blockChecker = mock[BlockCheckerActor[F]]
      val requestProxy = mock[RequestsProxyActor[F]]
      val mempool = mock[MempoolAlgebra[F]]

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
        defaultCache()
      initialCash.putAll(blockWithSource.asJava)

      val peer: PeerActor[F] = mockPeerActor[F]()
      val initialPeersMap: Map[HostId, Peer[F]] = Map(
        blockSource -> Peer(
          PeerState.Hot,
          Option(peer),
          blockSource,
          None,
          Seq.empty,
          remoteNetworkLevel = true,
          0,
          0,
          0
        ),
        coldPeer -> Peer(
          PeerState.Cold,
          Option(mockPeerActor[F]()),
          coldPeer,
          None,
          Seq.empty,
          remoteNetworkLevel = true,
          0,
          0,
          0
        ),
        preWarmPeer -> Peer(
          PeerState.Warm,
          Option(mockPeerActor[F]()),
          preWarmPeer,
          None,
          Seq.empty,
          remoteNetworkLevel = true,
          0,
          0,
          0
        ),
        bannedPeer -> Peer(
          PeerState.Banned,
          Option(mockPeerActor[F]()),
          bannedPeer,
          None,
          Seq.empty,
          remoteNetworkLevel = true,
          0,
          0,
          0
        ),
        warmPeer -> Peer(
          PeerState.Warm,
          Option(mockPeerActor[F]()),
          warmPeer,
          None,
          Seq.empty,
          remoteNetworkLevel = true,
          0,
          0,
          0
        )
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
          blockHeights,
          mempool,
          headerToBodyValidation,
          defaultTransactionSyntaxValidation,
          newPeerCreationAlgebra,
          defaultP2PConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          defaultWarmToHotSelector,
          initialPeersMap,
          initialCash
        )
        .use { actor =>
          for {
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
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] =
        mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val blockChecker = mock[BlockCheckerActor[F]]
      val requestProxy = mock[RequestsProxyActor[F]]
      val mempool = mock[MempoolAlgebra[F]]

      val blockHeader1 = ModelGenerators.arbitraryHeader.arbitrary.first.embedId
      val blockHeader2 = ModelGenerators.arbitraryHeader.arbitrary.first.embedId

      val blockSource: HostId = arbitraryHost.arbitrary.first

      val blockWithSource: Map[BlockId, Set[HostId]] = Map()

      val initialCash =
        defaultCache()
      initialCash.putAll(blockWithSource.asJava)

      val peer: PeerActor[F] = mockPeerActor[F]()
      val initialPeersMap: Map[HostId, Peer[F]] = Map(
        blockSource -> Peer(
          PeerState.Hot,
          Option(peer),
          blockSource,
          None,
          Seq.empty,
          remoteNetworkLevel = true,
          0,
          0,
          0
        )
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
          blockHeights,
          mempool,
          headerToBodyValidation,
          defaultTransactionSyntaxValidation,
          newPeerCreationAlgebra,
          defaultP2PConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          defaultWarmToHotSelector,
          initialPeersMap,
          initialCash
        )
        .use { actor =>
          for {
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
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] =
        mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val blockChecker = mock[BlockCheckerActor[F]]
      val requestProxy = mock[RequestsProxyActor[F]]
      val mempool = mock[MempoolAlgebra[F]]

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
        defaultCache()
      initialCash.putAll(blockWithSource.asJava)

      val initialPeersMap: Map[HostId, Peer[F]] = Map(
        coldPeer -> Peer(
          PeerState.Cold,
          Option(mockPeerActor[F]()),
          coldPeer,
          None,
          Seq.empty,
          remoteNetworkLevel = true,
          0,
          0,
          0
        ),
        preWarmPeer -> Peer(
          PeerState.Warm,
          Option(mockPeerActor[F]()),
          preWarmPeer,
          None,
          Seq.empty,
          remoteNetworkLevel = true,
          0,
          0,
          0
        ),
        bannedPeer -> Peer(
          PeerState.Banned,
          Option(mockPeerActor[F]()),
          bannedPeer,
          None,
          Seq.empty,
          remoteNetworkLevel = true,
          0,
          0,
          0
        ),
        warmPeer -> Peer(
          PeerState.Warm,
          Option(mockPeerActor[F]()),
          warmPeer,
          None,
          Seq.empty,
          remoteNetworkLevel = true,
          0,
          0,
          0
        )
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
          blockHeights,
          mempool,
          headerToBodyValidation,
          defaultTransactionSyntaxValidation,
          newPeerCreationAlgebra,
          defaultP2PConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          defaultWarmToHotSelector,
          initialPeersMap,
          initialCash
        )
        .use { actor =>
          for {
            _ <- actor.send(PeersManager.Message.SetupBlockChecker(blockChecker))
            _ <- actor.send(PeersManager.Message.SetupRequestsProxy(requestProxy))
            _ <- actor.send(messageToSend)
          } yield ()
        }

    }
  }

  test("Correct pong message shall be processed") {
    withMock {
      val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]
      val localChain: LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]]
      val slotDataStore: Store[F, BlockId, SlotData] = mock[Store[F, BlockId, SlotData]]
      val transactionStore: Store[F, TransactionId, IoTransaction] = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree: ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] =
        mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val blockChecker = mock[BlockCheckerActor[F]]
      val requestProxy = mock[RequestsProxyActor[F]]
      val mempool = mock[MempoolAlgebra[F]]

      val initBlock = 0.1
      val initNovelty = 1
      val initialPeersMap: Map[HostId, Peer[F]] = Map(
        hostId -> Peer(
          PeerState.Cold,
          Option(mockPeerActor[F]()),
          hostId,
          None,
          Seq.empty,
          remoteNetworkLevel = true,
          initBlock,
          0.5,
          initNovelty
        )
      )

      val delay = 230L
      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          blockHeights,
          mempool,
          headerToBodyValidation,
          defaultTransactionSyntaxValidation,
          newPeerCreationAlgebra,
          defaultP2PConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          defaultWarmToHotSelector,
          initialPeersMap,
          defaultCache()
        )
        .use { actor =>
          for {
            _        <- actor.send(PeersManager.Message.SetupBlockChecker(blockChecker))
            _        <- actor.send(PeersManager.Message.SetupRequestsProxy(requestProxy))
            newState <- actor.send(PeersManager.Message.PingPongMessagePing(hostId, Right(delay)))
            peer = newState.peersHandler(hostId)
            _ = assert(peer.perfRep == PeersManager.delayToReputation(defaultP2PConfig, delay))
            _ = assert(peer.blockRep == initBlock)
            _ = assert(peer.newRep == initNovelty)
          } yield ()
        }
    }
  }

  test("NoPongMessage message shall be processed") {
    withMock {
      val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]
      val localChain: LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]]
      val slotDataStore: Store[F, BlockId, SlotData] = mock[Store[F, BlockId, SlotData]]
      val transactionStore: Store[F, TransactionId, IoTransaction] = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree: ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] =
        mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val blockChecker = mock[BlockCheckerActor[F]]
      val requestProxy = mock[RequestsProxyActor[F]]
      val mempool = mock[MempoolAlgebra[F]]

      val initBlock = 0.1
      val initNovelty = 1
      val initialPeersMap: Map[HostId, Peer[F]] = Map(
        hostId -> Peer(
          PeerState.Cold,
          Option(mockPeerActor[F]()),
          hostId,
          None,
          Seq.empty,
          remoteNetworkLevel = true,
          initBlock,
          0.5,
          initNovelty
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
          blockHeights,
          mempool,
          headerToBodyValidation,
          defaultTransactionSyntaxValidation,
          newPeerCreationAlgebra,
          defaultP2PConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          defaultWarmToHotSelector,
          initialPeersMap,
          defaultCache()
        )
        .use { actor =>
          for {
            _        <- actor.send(PeersManager.Message.SetupBlockChecker(blockChecker))
            _        <- actor.send(PeersManager.Message.SetupRequestsProxy(requestProxy))
            newState <- actor.send(PeersManager.Message.PingPongMessagePing(hostId, Left(NoPongMessage)))
            peer = newState.peersHandler(hostId)
            _ = assert(peer.state == PeerState.Banned)
          } yield ()
        }
    }
  }

  test("IncorrectPongMessage message shall be processed") {
    withMock {
      val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]
      val localChain: LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]]
      val slotDataStore: Store[F, BlockId, SlotData] = mock[Store[F, BlockId, SlotData]]
      val transactionStore: Store[F, TransactionId, IoTransaction] = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree: ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] =
        mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val blockChecker = mock[BlockCheckerActor[F]]
      val requestProxy = mock[RequestsProxyActor[F]]
      val mempool = mock[MempoolAlgebra[F]]

      val initBlock = 0.1
      val initNovelty = 1
      val initialPeersMap: Map[HostId, Peer[F]] = Map(
        hostId -> Peer(
          PeerState.Cold,
          Option(mockPeerActor[F]()),
          hostId,
          None,
          Seq.empty,
          remoteNetworkLevel = true,
          initBlock,
          0.5,
          initNovelty
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
          blockHeights,
          mempool,
          headerToBodyValidation,
          defaultTransactionSyntaxValidation,
          newPeerCreationAlgebra,
          defaultP2PConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          defaultWarmToHotSelector,
          initialPeersMap,
          defaultCache()
        )
        .use { actor =>
          for {
            _        <- actor.send(PeersManager.Message.SetupBlockChecker(blockChecker))
            _        <- actor.send(PeersManager.Message.SetupRequestsProxy(requestProxy))
            newState <- actor.send(PeersManager.Message.PingPongMessagePing(hostId, Left(IncorrectPongMessage)))
            peer = newState.peersHandler(hostId)
            _ = assert(peer.state == PeerState.Banned)
          } yield ()
        }
    }
  }

  test("IncorrectBlock message shall be processed") {
    withMock {
      val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]
      val localChain: LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]]
      val slotDataStore: Store[F, BlockId, SlotData] = mock[Store[F, BlockId, SlotData]]
      val transactionStore: Store[F, TransactionId, IoTransaction] = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree: ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] =
        mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val blockChecker = mock[BlockCheckerActor[F]]
      val requestProxy = mock[RequestsProxyActor[F]]
      val mempool = mock[MempoolAlgebra[F]]

      val initBlock = 0.1
      val initNovelty = 1
      val initialPeersMap: Map[HostId, Peer[F]] = Map(
        hostId -> Peer(
          PeerState.Cold,
          Option(mockPeerActor[F]()),
          hostId,
          None,
          Seq.empty,
          remoteNetworkLevel = true,
          initBlock,
          0.5,
          initNovelty
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
          blockHeights,
          mempool,
          headerToBodyValidation,
          defaultTransactionSyntaxValidation,
          newPeerCreationAlgebra,
          defaultP2PConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          defaultWarmToHotSelector,
          initialPeersMap,
          defaultCache()
        )
        .use { actor =>
          for {
            _        <- actor.send(PeersManager.Message.SetupBlockChecker(blockChecker))
            _        <- actor.send(PeersManager.Message.SetupRequestsProxy(requestProxy))
            newState <- actor.send(PeersManager.Message.CriticalErrorForHost(hostId))
            peer = newState.peersHandler(hostId)
            _ = assert(peer.state == PeerState.Banned)
          } yield ()
        }
    }
  }

  test("Performance reputation after header downloading shall be updated") {
    withMock {
      val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]
      val localChain: LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]]
      val slotDataStore: Store[F, BlockId, SlotData] = mock[Store[F, BlockId, SlotData]]
      val transactionStore: Store[F, TransactionId, IoTransaction] = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree: ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] =
        mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val mempool = mock[MempoolAlgebra[F]]

      val initBlock = 0.1
      val initPerf = 0.5
      val initNovelty = 1
      val initialPeersMap: Map[HostId, Peer[F]] = Map(
        hostId -> Peer(
          PeerState.Cold,
          Option(mockPeerActor[F]()),
          hostId,
          None,
          Seq.empty,
          remoteNetworkLevel = true,
          initBlock,
          initPerf,
          initNovelty
        )
      )

      val downloadTime = 120
      val reputation = PeersManager.delayToReputation(defaultP2PConfig, downloadTime)

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          blockHeights,
          mempool,
          headerToBodyValidation,
          defaultTransactionSyntaxValidation,
          newPeerCreationAlgebra,
          defaultP2PConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          defaultWarmToHotSelector,
          initialPeersMap,
          defaultCache()
        )
        .use { actor =>
          for {
            newState <- actor.send(PeersManager.Message.DownloadTimeHeader(hostId, downloadTime))
            peer = newState.peersHandler(hostId)
            _ = assert(peer.perfRep == (initPerf * 2 + reputation) / 3.0)
            _ = assert(peer.blockRep == initBlock)
            _ = assert(peer.newRep == initNovelty)
          } yield ()
        }
    }
  }

  test("Performance reputation after body downloading shall be updated") {
    PropF.forAllF { (txTimes: Seq[Long]) =>
      withMock {
        val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]
        val localChain: LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]]
        val slotDataStore: Store[F, BlockId, SlotData] = mock[Store[F, BlockId, SlotData]]
        val transactionStore: Store[F, TransactionId, IoTransaction] = mock[Store[F, TransactionId, IoTransaction]]
        val blockIdTree: ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]
        val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
        val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] =
          mock[BlockHeaderToBodyValidationAlgebra[F]]
        val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
        val mempool = mock[MempoolAlgebra[F]]

        val initBlock = 0.1
        val initPerf = 0.5
        val initNovelty = 1
        val initialPeersMap: Map[HostId, Peer[F]] = Map(
          hostId -> Peer(
            PeerState.Cold,
            Option(mockPeerActor[F]()),
            hostId,
            None,
            Seq.empty,
            remoteNetworkLevel = true,
            initBlock,
            initPerf,
            initNovelty
          )
        )

        val downloadTime = 120L
        val txDownloadTime: Seq[Long] = txTimes.filter(_ > 0)
        val reputation = PeersManager.delayToReputation(defaultP2PConfig, (txDownloadTime :+ downloadTime).max)

        PeersManager
          .makeActor(
            thisHostId,
            networkAlgebra,
            localChain,
            slotDataStore,
            transactionStore,
            blockIdTree,
            blockHeights,
            mempool,
            headerToBodyValidation,
            defaultTransactionSyntaxValidation,
            newPeerCreationAlgebra,
            defaultP2PConfig,
            defaultHotPeerUpdater,
            defaultPeersSaver,
            defaultColdToWarmSelector,
            defaultWarmToHotSelector,
            initialPeersMap,
            defaultCache()
          )
          .use { actor =>
            for {
              newState <- actor.send(PeersManager.Message.DownloadTimeBody(hostId, downloadTime, txDownloadTime))
              peer = newState.peersHandler(hostId)
              _ = assert(peer.perfRep == (initPerf * 2 + reputation) / 3.0)
              _ = assert(peer.blockRep == initBlock)
              _ = assert(peer.newRep == initNovelty)
            } yield ()
          }
      }
    }
  }

  test("Block providing reputation: take better value from update and current value") {
    withMock {
      val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]
      val localChain: LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]]
      val slotDataStore: Store[F, BlockId, SlotData] = mock[Store[F, BlockId, SlotData]]
      val transactionStore: Store[F, TransactionId, IoTransaction] = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree: ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] =
        mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val mempool = mock[MempoolAlgebra[F]]

      val host1 = arbitraryHost.arbitrary.first
      val host2 = arbitraryHost.arbitrary.first
      val host3 = arbitraryHost.arbitrary.first
      val host4 = arbitraryHost.arbitrary.first
      val host5 = arbitraryHost.arbitrary.first

      val blockId1 = arbitraryBlockId.arbitrary.first
      val blockId2 = arbitraryBlockId.arbitrary.first
      val blockId3 = arbitraryBlockId.arbitrary.first
      val blockId4 = arbitraryBlockId.arbitrary.first
      val blockId5 = arbitraryBlockId.arbitrary.first

      val worstedKnownSources = 4L
      val worstedReputation = PeersManager.knownSourcesToReputation(defaultP2PConfig, worstedKnownSources)

      val worstKnownSources = 3L
      val worstReputation = PeersManager.knownSourcesToReputation(defaultP2PConfig, worstKnownSources)

      val okKnownSources = 2L
      val okReputation = PeersManager.knownSourcesToReputation(defaultP2PConfig, okKnownSources)

      val bestKnownSources = 1L
      val bestReputation = PeersManager.knownSourcesToReputation(defaultP2PConfig, bestKnownSources)
      assert(bestReputation == 1)

      val update: NonEmptyChain[(HostId, BlockId)] = NonEmptyChain(
        host1 -> blockId1,
        host1 -> blockId2,
        host1 -> blockId3,
        host1 -> blockId4,
        host2 -> blockId2,
        host2 -> blockId3,
        host2 -> blockId4,
        host3 -> blockId3,
        host3 -> blockId4,
        host4 -> blockId4,
        host5 -> blockId5
      )

      val initialPeersMap: Map[HostId, Peer[F]] = Map(
        host1 -> Peer(
          PeerState.Hot,
          Option(mockPeerActor[F]()),
          host1,
          None,
          Seq.empty,
          remoteNetworkLevel = true,
          worstReputation,
          0.0,
          0
        ),
        host2 -> Peer(
          PeerState.Hot,
          Option(mockPeerActor[F]()),
          host2,
          None,
          Seq.empty,
          remoteNetworkLevel = true,
          worstReputation,
          0.0,
          0
        ),
        host3 -> Peer(
          PeerState.Hot,
          Option(mockPeerActor[F]()),
          host3,
          None,
          Seq.empty,
          remoteNetworkLevel = true,
          worstReputation,
          0.0,
          0
        ),
        host4 -> Peer(
          PeerState.Hot,
          Option(mockPeerActor[F]()),
          host4,
          None,
          Seq.empty,
          remoteNetworkLevel = true,
          worstReputation,
          0.0,
          0
        ),
        host5 -> Peer(
          PeerState.Hot,
          Option(mockPeerActor[F]()),
          host5,
          None,
          Seq.empty,
          remoteNetworkLevel = true,
          worstedReputation,
          0.0,
          0
        )
      )

      val cache = Caffeine.newBuilder.maximumSize(blockSourceCacheSize).build[BlockId, Set[HostId]]()
      cache.put(blockId5, Set(host1, host2, host3))

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          blockHeights,
          mempool,
          headerToBodyValidation,
          defaultTransactionSyntaxValidation,
          newPeerCreationAlgebra,
          defaultP2PConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          defaultWarmToHotSelector,
          initialPeersMap,
          cache
        )
        .use { actor =>
          for {
            res <- actor.send(PeersManager.Message.BlocksSource(update))
            _ = assert(res.peersHandler(host1).blockRep == bestReputation)
            _ = assert(res.peersHandler(host2).blockRep == okReputation)
            _ = assert(res.peersHandler(host3).blockRep == worstReputation)
            _ = assert(res.peersHandler(host4).blockRep == worstReputation)
            _ = assert(res.peersHandler(host5).blockRep == worstedReputation)
          } yield ()
        }
    }
  }

  test("Move remote peer to cold if remote host provide bad k lookback slot data") {
    withMock {
      val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]
      val localChain: LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]]
      val slotDataStore: Store[F, BlockId, SlotData] = mock[Store[F, BlockId, SlotData]]
      val transactionStore: Store[F, TransactionId, IoTransaction] = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree: ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] =
        mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val mempool = mock[MempoolAlgebra[F]]

      val peerActor = mockPeerActor[F]()
      val initialPeersMap: Map[HostId, Peer[F]] = Map(
        hostId -> Peer(
          PeerState.Hot,
          Option(peerActor),
          hostId,
          None,
          Seq.empty,
          remoteNetworkLevel = true,
          0,
          0.0,
          0
        )
      )

      (peerActor.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = false, applicationLevel = false))
        .returns(Applicative[F].unit)

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          blockHeights,
          mempool,
          headerToBodyValidation,
          defaultTransactionSyntaxValidation,
          newPeerCreationAlgebra,
          defaultP2PConfig,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          defaultWarmToHotSelector,
          initialPeersMap,
          defaultCache()
        )
        .use { actor =>
          for {
            newState <- actor.send(PeersManager.Message.BadKLookbackSlotData(hostId))
            _ = assert(newState.peersHandler(hostId).state == PeerState.Cold)
          } yield ()
        }
    }
  }

  test("Cold peers shall be cleared") {
    withMock {
      val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]
      val localChain: LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]]
      val slotDataStore: Store[F, BlockId, SlotData] = mock[Store[F, BlockId, SlotData]]
      val transactionStore: Store[F, TransactionId, IoTransaction] = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree: ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]]
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] =
        mock[BlockHeaderToBodyValidationAlgebra[F]]
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
      val mempool = mock[MempoolAlgebra[F]]

      val host1 = "1"
      val host2 = "2"
      val host3 = "3"
      val host4 = "4"
      val host5 = "5"
      val host6 = "6"

      val initialPeersMap: Map[HostId, Peer[F]] = Map(
        host1 -> Peer(
          PeerState.Cold,
          None,
          host1,
          None,
          Seq.empty,
          remoteNetworkLevel = true,
          0,
          0.0,
          0
        ),
        host2 -> Peer(
          PeerState.Cold,
          None,
          host2,
          2.some,
          Seq(0),
          remoteNetworkLevel = true,
          0,
          0.0,
          0
        ),
        host3 -> Peer(
          PeerState.Cold,
          None,
          host3,
          3.some,
          Seq(0, 1),
          remoteNetworkLevel = true,
          0,
          0.0,
          0
        ),
        host4 -> Peer(
          PeerState.Cold,
          None,
          host4,
          4.some,
          Seq(0, 1),
          remoteNetworkLevel = true,
          0,
          0.0,
          0
        ),
        host5 -> Peer(
          PeerState.Cold,
          None,
          host5,
          5.some,
          Seq(System.currentTimeMillis()),
          remoteNetworkLevel = true,
          0,
          0.0,
          0
        ),
        host6 -> Peer(
          PeerState.Cold,
          None,
          host6,
          6.some,
          Seq(System.currentTimeMillis()),
          remoteNetworkLevel = true,
          0,
          0.0,
          0
        )
      )

      val networkConfig =
        defaultP2PConfig.networkProperties.copy(
          maximumEligibleColdConnections = 2,
          minimumEligibleColdConnections = 1,
          closeTimeoutFirstDelayInMs = 1000000
        )

      val config = defaultP2PConfig.copy(networkProperties = networkConfig)
      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          blockHeights,
          mempool,
          headerToBodyValidation,
          defaultTransactionSyntaxValidation,
          newPeerCreationAlgebra,
          config,
          defaultHotPeerUpdater,
          defaultPeersSaver,
          defaultColdToWarmSelector,
          defaultWarmToHotSelector,
          initialPeersMap,
          defaultCache()
        )
        .use { actor =>
          for {
            newState <- actor.send(PeersManager.Message.UpdateWarmHosts)
            _ = assert(newState.peersHandler.getColdPeers.size == 4) // 1 saved + non eligible 3 cold peers
            _ = assert(newState.peersHandler.getColdPeers.contains(host2))
            _ = assert(newState.peersHandler.getColdPeers.contains(host1))
            _ = assert(newState.peersHandler.getColdPeers.contains(host5))
            _ = assert(newState.peersHandler.getColdPeers.contains(host6))
          } yield ()
        }
    }
  }
}
