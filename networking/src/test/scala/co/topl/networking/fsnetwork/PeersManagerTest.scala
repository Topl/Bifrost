package co.topl.networking.fsnetwork

import cats.{Applicative, Parallel}
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
import co.topl.networking.fsnetwork.TestHelper.{arbitraryHost, arbitraryRemoteAddress}
import co.topl.networking.p2p.{ConnectedPeer, DisconnectedPeer, RemoteAddress}
import co.topl.node.models.{BlockBody, KnownHost}
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

  val thisHostId: HostId = arbitraryHost.arbitrary.first
  val hostId: HostId = arbitraryHost.arbitrary.first

  val defaultColdToWarmSelector: SelectorColdToWarm[F] =
    (coldHosts: Map[HostId, Peer[F]], countToReceive: Int) =>
      coldHosts.toSeq.sortBy(_._2.asServer.map(_.address.port)).take(countToReceive).map(_._1).toSet

  def defaultBodyStorage: Store[F, BlockId, BlockBody] = mock[Store[F, BlockId, BlockBody]]

  val defaultWarmToHotSelector: SelectorWarmToHot[F] =
    new SelectorWarmToHot[F]() {

      override def select(hosts: Map[HostId, Peer[F]], countToReceive: Int): Set[HostId] =
        hosts.toSeq.sortBy(_._2.reputation).takeRight(countToReceive).map(_._1).toSet
    }

  val defaultHotPeerUpdater: Set[RemotePeer] => F[Unit] = _ => Applicative[F].unit
  val defaultPeersSaver: Set[KnownRemotePeer] => F[Unit] = _ => Applicative[F].unit

  val defaultP2PConfig: P2PNetworkConfig =
    P2PNetworkConfig(NetworkProperties(closeTimeoutWindowInMs = Long.MaxValue), FiniteDuration(1, SECONDS))

  val defaultTransactionSyntaxValidation: TransactionSyntaxVerifier[F] = (t: IoTransaction) => Either.right(t).pure[F]

  def defaultCache(): Cache[BlockId, Set[HostId]] =
    Caffeine.newBuilder.maximumSize(blockSourceCacheSize).build[BlockId, Set[HostId]]()

  implicit val dummyDns: DnsResolver[F] = (host: String) => Option(host).pure[F]
  implicit val dummyReverseDns: ReverseDnsResolver[F] = (h: String) => h.pure[F]

  def asServer(ra: RemoteAddress, id: HostId = arbitraryHost.arbitrary.first): Option[RemotePeer] =
    RemotePeer(id, ra).some

  def mockPeerActor[F[_]: Applicative](): PeerActor[F] = {
    val mocked = mock[PeerActor[F]]
    (() => mocked.id).stubs().returns(mocked.hashCode())
    (mocked.mailboxSize _).stubs().returns(0.pure[F])

    mocked
  }

  def buildOpenedPeerConnectionMessage[F[_]: Applicative](
    client:        BlockchainPeerClient[F],
    connectedPeer: ConnectedPeer
  ): PeersManager.Message.OpenedPeerConnection[F] = {
    (() => client.remotePeer).expects().anyNumberOfTimes().returns(connectedPeer.pure[F])

    val asServer = KnownHost(connectedPeer.p2pVK, connectedPeer.remoteAddress.host, connectedPeer.remoteAddress.port)
    (() => client.remotePeerAsServer).stubs().returns(asServer.some.pure[F])
    PeersManager.Message.OpenedPeerConnection(client)
  }

  def buildSimplePeerEntry(
    state:              PeerState,
    actorOpt:           Option[PeerActor[F]] = None,
    id:                 HostId = arbitraryHost.arbitrary.first,
    address:            RemoteAddress = arbitraryRemoteAddress.arbitrary.first,
    closedTimestamps:   Seq[Long] = Seq.empty,
    remoteNetworkLevel: Boolean = true,
    blockRep:           HostReputationValue = 0,
    perfRep:            HostReputationValue = 0,
    newRep:             Long = 0,
    noServer:           Boolean = false
  ): (HostId, Peer[F]) = buildPeerEntry(
    id,
    state,
    if (noServer) None else address.some,
    address.some,
    actorOpt,
    closedTimestamps,
    remoteNetworkLevel,
    blockRep,
    perfRep,
    newRep
  )

  def buildPeerEntry(
    id:                 HostId,
    state:              PeerState,
    serverAddress:      Option[RemoteAddress],
    connectedAddress:   Option[RemoteAddress],
    actorOpt:           Option[PeerActor[F]] = None,
    closedTimestamps:   Seq[Long] = Seq.empty,
    remoteNetworkLevel: Boolean = true,
    blockRep:           HostReputationValue = 0,
    perfRep:            HostReputationValue = 0,
    newRep:             Long = 0
  ): (HostId, Peer[F]) =
    id -> Peer(
      state,
      actorOpt,
      connectedAddress,
      serverAddress.map(sa => RemotePeer(id, sa)),
      closedTimestamps,
      remoteNetworkLevel,
      blockRep,
      perfRep,
      newRep
    )

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

      val coldPeer = mockPeerActor[F]()

      val warmPeer = mockPeerActor[F]()

      val hotPeer = mockPeerActor[F]()
      (hotPeer.sendNoWait _)
        .expects(PeerActor.Message.GetCurrentTip)
        .returns(().pure[F])

      val banedPeer = mockPeerActor[F]()

      val initialPeersMap =
        Map[HostId, Peer[F]](
          buildSimplePeerEntry(PeerState.Cold, Option(coldPeer)),
          buildSimplePeerEntry(PeerState.Warm, Option(warmPeer)),
          buildSimplePeerEntry(PeerState.Hot, Option(hotPeer)),
          buildSimplePeerEntry(PeerState.Banned, Option(banedPeer))
        )

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          defaultBodyStorage,
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

      arbitraryHost.arbitrary.first
      val coldPeer = mockPeerActor[F]()

      arbitraryHost.arbitrary.first
      val warmPeer = mockPeerActor[F]()
      (warmPeer.sendNoWait _)
        .expects(PeerActor.Message.GetNetworkQuality)
        .returns(().pure[F])

      arbitraryHost.arbitrary.first
      val hotPeer = mockPeerActor[F]()

      arbitraryHost.arbitrary.first
      val banedPeer = mockPeerActor[F]()

      val initialPeersMap =
        Map[HostId, Peer[F]](
          buildSimplePeerEntry(PeerState.Cold, Option(coldPeer)),
          buildSimplePeerEntry(PeerState.Warm, Option(warmPeer)),
          buildSimplePeerEntry(PeerState.Hot, Option(hotPeer)),
          buildSimplePeerEntry(PeerState.Banned, Option(banedPeer))
        )

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          defaultBodyStorage,
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

      import co.topl.models.generators.consensus.ModelGenerators.arbitrarySlotData
      (() => localChain.head).expects().once().returns(arbitrarySlotData.arbitrary.first.pure[F])
      arbitraryHost.arbitrary.first
      val coldPeer = mockPeerActor[F]()
      (coldPeer.sendNoWait _)
        .expects(PeerActor.Message.PrintCommonAncestor)
        .returns(().pure[F])

      arbitraryHost.arbitrary.first
      val warmPeer = mockPeerActor[F]()
      (warmPeer.sendNoWait _)
        .expects(PeerActor.Message.PrintCommonAncestor)
        .returns(().pure[F])

      arbitraryHost.arbitrary.first

      arbitraryHost.arbitrary.first

      val initialPeersMap =
        Map[HostId, Peer[F]](
          buildSimplePeerEntry(PeerState.Cold, Option(coldPeer)),
          buildSimplePeerEntry(PeerState.Warm, Option(warmPeer)),
          buildSimplePeerEntry(PeerState.Hot),
          buildSimplePeerEntry(PeerState.Banned)
        )

      val requestsProxy = mock[RequestsProxyActor[F]]
      (() => requestsProxy.mailboxSize()).expects().once().returns(0.pure[F])

      val blockChecker = mock[BlockCheckerActor[F]]
      (() => blockChecker.mailboxSize()).expects().once().returns(0.pure[F])

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          defaultBodyStorage,
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
            _ <- actor.send(PeersManager.Message.SetupRequestsProxy(requestsProxy))
            _ <- actor.send(PeersManager.Message.SetupBlockChecker(blockChecker))
            _ <- actor.send(PeersManager.Message.PrintP2PState)
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

      val host1 = arbitraryHost.arbitrary.first
      val peerActor1 = mockPeerActor[F]()
      (peerActor1.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = false, applicationLevel = false))
        .returns(().pure[F])

      val host2 = arbitraryHost.arbitrary.first
      val peerActor2 = mockPeerActor[F]()

      val initialPeersMap =
        Map(
          buildSimplePeerEntry(PeerState.Hot, Option(peerActor1), host1),
          buildSimplePeerEntry(PeerState.Cold, Option(peerActor2), host2, remoteNetworkLevel = false)
        )

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          defaultBodyStorage,
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
        Map.empty[HostId, Peer[F]] + buildSimplePeerEntry(
          PeerState.Hot,
          Option(peerActor),
          hostId,
          closedTimestamps = Seq(System.currentTimeMillis())
        )

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          defaultBodyStorage,
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
          buildSimplePeerEntry(
            PeerState.Hot,
            Option(peerActor1),
            host1,
            closedTimestamps = Seq(System.currentTimeMillis()),
            remoteNetworkLevel = false
          ),
          buildSimplePeerEntry(
            PeerState.Warm,
            Option(peerActor2),
            host2,
            closedTimestamps = Seq(System.currentTimeMillis())
          )
        )

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          defaultBodyStorage,
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
        Map.empty[HostId, Peer[F]] + (buildSimplePeerEntry(
          PeerState.Hot,
          Option(peerActor),
          hostId,
          closedTimestamps = Seq(0, 200, System.currentTimeMillis() - timeoutWindows - 1, System.currentTimeMillis())
        ))

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          defaultBodyStorage,
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
            _ = assert(endState.peersHandler(hostId).closedTimestamps.size == 5)
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

      val host1Ra = RemoteAddress("1", 1)
      val host1Id = arbitraryHost.arbitrary.first
      val host1 = DisconnectedPeer(host1Ra, host1Id.id.some)

      val host2Ra = RemoteAddress("2", 2)
      val host2Id = arbitraryHost.arbitrary.first
      val host2 = DisconnectedPeer(host2Ra, host2Id.id.some)

      val host3Ra = RemoteAddress("3", 3)
      val host3Id = arbitraryHost.arbitrary.first
      DisconnectedPeer(host3Ra, host3Id.id.some)

      (newPeerCreationAlgebra.requestNewPeerCreation _).expects(host1).returns(().pure[F])
      (newPeerCreationAlgebra.requestNewPeerCreation _).expects(host2).returns(().pure[F])

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          defaultBodyStorage,
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
          val peers =
            NonEmptyChain(
              KnownRemotePeer(host1Id, host1Ra, 0, 0),
              KnownRemotePeer(host2Id, host2Ra, 0, 0),
              KnownRemotePeer(host3Id, host3Ra, 0, 0)
            )
          for {
            withColdPeer <- actor.send(PeersManager.Message.AddKnownPeers(peers))
            _ = assert(withColdPeer.peersHandler(host1Id).state == PeerState.Cold)
            _ = assert(withColdPeer.peersHandler(host2Id).state == PeerState.Cold)
            _ = assert(withColdPeer.peersHandler(host3Id).state == PeerState.Cold)

            withUpdate <- actor.send(PeersManager.Message.UpdatedReputationTick)
            _ = assert(withUpdate.peersHandler(host1Id).state == PeerState.Warm)
            _ = assert(withUpdate.peersHandler(host2Id).state == PeerState.Warm)
            _ = assert(withUpdate.peersHandler(host3Id).state == PeerState.Cold)
          } yield ()
        }
    }
  }

  test("Reputation update: If no warm peer then move eligible cold peer(s) with port or actor to warm") {
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

      val host1Id = arbitraryHost.arbitrary.first
      val host1Ra = RemoteAddress("1", 1)
      val host2Id = arbitraryHost.arbitrary.first
      val host2Ra = RemoteAddress("2", 2)
      val host3Id = arbitraryHost.arbitrary.first
      val host3Ra = RemoteAddress("3", 3)
      val host4Id = arbitraryHost.arbitrary.first
      val host4Ra = RemoteAddress("4", 4)
      val host5Id = arbitraryHost.arbitrary.first
      val host5Ra = RemoteAddress("5", 5)
      val host6Id = arbitraryHost.arbitrary.first
      val host6Ra = RemoteAddress("6", 6)
      val host7Id = arbitraryHost.arbitrary.first
      val host7Ra = RemoteAddress("7", 6)
      val host7Actor = mock[PeerActor[F]]
      (host7Actor.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = true, applicationLevel = false))
        .returning(Applicative[F].unit)

      (newPeerCreationAlgebra.requestNewPeerCreation _)
        .expects(DisconnectedPeer(host1Ra, host1Id.id.some))
        .returns(().pure[F])
      (newPeerCreationAlgebra.requestNewPeerCreation _)
        .expects(DisconnectedPeer(host2Ra, host2Id.id.some))
        .returns(().pure[F])

      val initialPeersMap: Map[HostId, Peer[F]] =
        Map(
          buildSimplePeerEntry(PeerState.Cold, None, host1Id, host1Ra),
          buildSimplePeerEntry(PeerState.Cold, None, host2Id, host2Ra),
          buildPeerEntry(host3Id, PeerState.Cold, None, host3Ra.some, None),
          buildSimplePeerEntry(PeerState.Warm, None, host4Id, host4Ra),
          buildSimplePeerEntry(PeerState.Hot, None, host5Id, host5Ra),
          buildSimplePeerEntry(PeerState.Banned, None, host6Id, host6Ra),
          buildSimplePeerEntry(PeerState.Cold, host7Actor.some, host7Id, host7Ra)
        )

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          defaultBodyStorage,
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
            _ = assert(withUpdate.peersHandler(host1Id).state == PeerState.Warm)
            _ = assert(withUpdate.peersHandler(host2Id).state == PeerState.Warm)
            _ = assert(withUpdate.peersHandler(host3Id).state == PeerState.Cold)
            _ = assert(withUpdate.peersHandler(host4Id).state == PeerState.Warm)
            _ = assert(withUpdate.peersHandler(host5Id).state == PeerState.Hot)
            _ = assert(withUpdate.peersHandler(host6Id).state == PeerState.Banned)
            _ = assert(withUpdate.peersHandler(host7Id).state == PeerState.Warm)
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

      val host1Id = arbitraryHost.arbitrary.first
      val host1Ra = RemoteAddress("1", 1)
      val host2Id = arbitraryHost.arbitrary.first
      val host2Ra = RemoteAddress("2", 2)
      val host3Id = arbitraryHost.arbitrary.first
      val host3Ra = RemoteAddress("3", 3)

      val initialPeersMap: Map[HostId, Peer[F]] =
        Map(
          buildSimplePeerEntry(PeerState.Cold, None, host1Id, host1Ra, blockRep = 0.7, perfRep = 0.8, newRep = 0),
          buildSimplePeerEntry(PeerState.Cold, None, host2Id, host2Ra, blockRep = 0.8, perfRep = 0.2, newRep = 0),
          buildSimplePeerEntry(PeerState.Cold, None, host3Id, host3Ra, blockRep = 0.0, perfRep = 0.0, newRep = 0)
        )

      (newPeerCreationAlgebra.requestNewPeerCreation _)
        .expects(DisconnectedPeer(host1Ra, host1Id.id.some))
        .returns(().pure[F])
      (newPeerCreationAlgebra.requestNewPeerCreation _)
        .expects(DisconnectedPeer(host2Ra, host2Id.id.some))
        .returns(().pure[F])

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          defaultBodyStorage,
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
            _ = assert(withUpdate.peersHandler(host1Id).state == PeerState.Warm)
            _ = assert(withUpdate.peersHandler(host2Id).state == PeerState.Warm)
            _ = assert(withUpdate.peersHandler(host3Id).state == PeerState.Cold)
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

      val host1Id = arbitraryHost.arbitrary.first
      val host1Ra = RemoteAddress("1", 1)
      val host2Id = arbitraryHost.arbitrary.first
      val host2Ra = RemoteAddress("2", 2)
      val host3Id = arbitraryHost.arbitrary.first
      val host3Ra = RemoteAddress("3", 3)

      val peer1 = mockPeerActor[F]()
      (peer1.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = true, applicationLevel = true))
        .returns(().pure[F])

      val peer2 = mockPeerActor[F]()

      val initialPeersMap: Map[HostId, Peer[F]] =
        Map(
          buildSimplePeerEntry(PeerState.Warm, peer1.some, host1Id, host1Ra, blockRep = 0.1),
          buildSimplePeerEntry(PeerState.Warm, peer2.some, host2Id, host2Ra, blockRep = 0.0),
          buildSimplePeerEntry(PeerState.Warm, None, host3Id, host3Ra, blockRep = 0.1)
        )

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          defaultBodyStorage,
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
            _ = assert(withUpdate.peersHandler(host1Id).state == PeerState.Hot)
            _ = assert(withUpdate.peersHandler(host2Id).state == PeerState.Warm)
            _ = assert(withUpdate.peersHandler(host3Id).state == PeerState.Warm)
            _ = assert(withUpdate.peersHandler.get(host1Id).get.newRep == p2pConfig.remotePeerNoveltyInSlots)
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

      val host1Id = arbitraryHost.arbitrary.first
      val host1Ra = RemoteAddress("1", 1)
      val host2Id = arbitraryHost.arbitrary.first
      val host2Ra = RemoteAddress("2", 2)
      val host3Id = arbitraryHost.arbitrary.first
      val host3Ra = RemoteAddress("3", 3)

      val repBlock = 0.8
      val repPerf = 0.9
      val repNovelty = 2
      val initialPeersMap: Map[HostId, Peer[F]] =
        Map(
          buildSimplePeerEntry(
            PeerState.Cold,
            None,
            host1Id,
            host1Ra,
            blockRep = repBlock,
            perfRep = repPerf,
            newRep = repNovelty
          ),
          buildSimplePeerEntry(
            PeerState.Warm,
            None,
            host2Id,
            host2Ra,
            blockRep = repBlock,
            perfRep = repPerf,
            newRep = repNovelty
          ),
          buildSimplePeerEntry(
            PeerState.Hot,
            None,
            host3Id,
            host3Ra,
            blockRep = repBlock,
            perfRep = repPerf,
            newRep = repNovelty
          )
        )

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          defaultBodyStorage,
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
            _ = assert(withUpdate.peersHandler(host1Id).state == PeerState.Cold)
            _ = assert(withUpdate.peersHandler(host2Id).state == PeerState.Warm)
            _ = assert(withUpdate.peersHandler(host3Id).state == PeerState.Hot)
            _ = assert(peers(host1Id).perfRep == repPerf)
            _ = assert(peers(host1Id).blockRep == repBlock)
            _ = assert(peers(host1Id).newRep == repNovelty)
            _ = assert(peers(host2Id).perfRep == repPerf)
            _ = assert(peers(host2Id).blockRep == repBlock)
            _ = assert(peers(host2Id).newRep == repNovelty)
            _ = assert(peers(host3Id).perfRep == repPerf)
            _ = assert(peers(host3Id).blockRep == repBlock * defaultP2PConfig.blockNoveltyDecoy)
            _ = assert(peers(host3Id).newRep == repNovelty - 1)
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

      val host1Id = arbitraryHost.arbitrary.first
      val host1Ra = RemoteAddress("1", 1)
      val host2Id = arbitraryHost.arbitrary.first
      val host2Ra = RemoteAddress("2", 2)
      val host3Id = arbitraryHost.arbitrary.first
      val host3Ra = RemoteAddress("3", 3)
      val host4Id = arbitraryHost.arbitrary.first
      val host4Ra = RemoteAddress("4", 4)
      val host5Id = arbitraryHost.arbitrary.first
      val host5Ra = RemoteAddress("5", 5)
      val host6Id = arbitraryHost.arbitrary.first
      val host6Ra = RemoteAddress("6", 6)
      val host7Id = arbitraryHost.arbitrary.first
      val host7Ra = RemoteAddress("7", 7)

      (newPeerCreationAlgebra.requestNewPeerCreation _)
        .expects(DisconnectedPeer(host1Ra, host1Id.id.some))
        .returns(().pure[F])
      (newPeerCreationAlgebra.requestNewPeerCreation _)
        .expects(DisconnectedPeer(host2Ra, host2Id.id.some))
        .returns(().pure[F])
      (newPeerCreationAlgebra.requestNewPeerCreation _)
        .expects(DisconnectedPeer(host3Ra, host3Id.id.some))
        .returns(().pure[F])

      val closeTimeoutFirstDelayInMs = p2pConfig.networkProperties.closeTimeoutFirstDelayInMs

      val initialPeersMap: Map[HostId, Peer[F]] =
        Map(
          buildSimplePeerEntry(PeerState.Cold, None, host1Id, host1Ra),
          buildSimplePeerEntry(
            PeerState.Cold,
            None,
            host2Id,
            host2Ra,
            closedTimestamps = Seq(System.currentTimeMillis() - closeTimeoutFirstDelayInMs)
          ),
          buildSimplePeerEntry(
            PeerState.Cold,
            None,
            host3Id,
            host3Ra,
            closedTimestamps = Seq(0, System.currentTimeMillis() - (4 * closeTimeoutFirstDelayInMs))
          ),
          buildSimplePeerEntry(
            PeerState.Cold,
            None,
            host4Id,
            host4Ra,
            closedTimestamps = Seq(System.currentTimeMillis())
          ),
          buildSimplePeerEntry(
            PeerState.Cold,
            None,
            host5Id,
            host5Ra,
            closedTimestamps = Seq(0, System.currentTimeMillis() - closeTimeoutFirstDelayInMs)
          ),
          buildSimplePeerEntry(
            PeerState.Cold,
            None,
            host6Id,
            host6Ra,
            closedTimestamps = Seq(0, 1, System.currentTimeMillis() - (4 * closeTimeoutFirstDelayInMs))
          ),
          buildPeerEntry(host7Id, PeerState.Cold, None, host7Ra.some, closedTimestamps = Seq.empty)
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
          defaultBodyStorage,
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
            _ = assert(withUpdate.peersHandler(host1Id).state == PeerState.Warm)
            _ = assert(withUpdate.peersHandler(host2Id).state == PeerState.Warm)
            _ = assert(withUpdate.peersHandler(host3Id).state == PeerState.Warm)
            _ = assert(withUpdate.peersHandler(host4Id).state == PeerState.Cold)
            _ = assert(withUpdate.peersHandler(host5Id).state == PeerState.Cold)
            _ = assert(withUpdate.peersHandler(host6Id).state == PeerState.Cold)
            _ = assert(withUpdate.peersHandler(host7Id).state == PeerState.Cold)
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

      val host1Id = arbitraryHost.arbitrary.first
      val host1Ra = RemoteAddress("1", 1)
      val host2Id = arbitraryHost.arbitrary.first
      val host2Ra = RemoteAddress("2", 2)
      val host3Id = arbitraryHost.arbitrary.first
      val host3Ra = RemoteAddress("3", 3)
      val host4Id = arbitraryHost.arbitrary.first
      val host4Ra = RemoteAddress("4", 4)

      val initialPeersMap: Map[HostId, Peer[F]] =
        Map(
          buildSimplePeerEntry(PeerState.Hot, None, host1Id, host1Ra),
          buildSimplePeerEntry(PeerState.Banned, None, host2Id, host2Ra),
          buildSimplePeerEntry(PeerState.Cold, None, host3Id, host3Ra)
        )

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          defaultBodyStorage,
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
          val peers: NonEmptyChain[KnownRemotePeer] = NonEmptyChain(
            KnownRemotePeer(host1Id, host1Ra, 0, 0),
            KnownRemotePeer(host2Id, host2Ra, 0, 0),
            KnownRemotePeer(host3Id, host3Ra, 0, 0),
            KnownRemotePeer(host4Id, host4Ra, 0, 0)
          )

          for {
            withColdPeer <- actor.send(PeersManager.Message.AddKnownPeers(peers))
            _ = assert(withColdPeer.peersHandler(host1Id).state == PeerState.Hot)
            _ = assert(withColdPeer.peersHandler(host2Id).state == PeerState.Banned)
            _ = assert(withColdPeer.peersHandler(host3Id).state == PeerState.Cold)
            _ = assert(withColdPeer.peersHandler(host4Id).state == PeerState.Cold)
            _ = assert(withColdPeer.peersHandler(host4Id).closedTimestamps == Seq.empty)

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

      val host1Id = arbitraryHost.arbitrary.first
      val host1Ra = RemoteAddress("first", 1)
      val peer1 = mockPeerActor[F]()

      val host2Id = arbitraryHost.arbitrary.first
      val host2Ra = RemoteAddress("second", 2)
      val peer2 = mockPeerActor[F]()

      val host3Id = arbitraryHost.arbitrary.first
      val host3Ra = RemoteAddress("third", 3)
      val peer3 = mockPeerActor[F]()

      val host4Id = arbitraryHost.arbitrary.first
      val host4Ra = RemoteAddress("fourth", 4)
      val peer4 = mockPeerActor[F]()

      val host5Id = arbitraryHost.arbitrary.first
      val host5Ra = RemoteAddress("five", 5)
      val peer5 = mockPeerActor[F]()
      (peer5.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = false, applicationLevel = false))
        .returns(().pure[F])

      val host6Id = arbitraryHost.arbitrary.first
      val host6Ra = RemoteAddress("six", 6)
      val peer6 = mockPeerActor[F]()
      (peer6.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = false, applicationLevel = false))
        .returns(().pure[F])

      val initialPeersMap: Map[HostId, Peer[F]] =
        Map(
          buildSimplePeerEntry(
            PeerState.Hot,
            Option(peer1),
            host1Id,
            host1Ra,
            closedTimestamps = Seq(1),
            blockRep = 0,
            perfRep = 1.0,
            newRep = 0
          ),
          buildPeerEntry(
            host2Id,
            PeerState.Hot,
            None,
            host2Ra.some,
            Option(peer2),
            closedTimestamps = Seq(2),
            blockRep = 1.0,
            perfRep = 0,
            newRep = 0
          ),
          buildSimplePeerEntry(
            PeerState.Hot,
            Option(peer3),
            host3Id,
            host3Ra,
            closedTimestamps = Seq(3),
            blockRep = defaultP2PConfig.networkProperties.minimumRequiredReputation * 1.05,
            perfRep = defaultP2PConfig.networkProperties.minimumRequiredReputation * 1.05,
            newRep = 0
          ),
          buildPeerEntry(
            host4Id,
            PeerState.Hot,
            None,
            host4Ra.some,
            Option(peer4),
            closedTimestamps = Seq(4),
            blockRep = 0,
            perfRep = 0,
            newRep = 2
          ),
          buildPeerEntry(
            host5Id,
            PeerState.Hot,
            None,
            host5Ra.some,
            Option(peer5),
            closedTimestamps = Seq(5),
            blockRep = 0,
            perfRep = 0,
            newRep = 0,
            remoteNetworkLevel = false
          ),
          buildPeerEntry(
            host6Id,
            PeerState.Hot,
            None,
            host6Ra.some,
            Option(peer6),
            closedTimestamps = Seq(6),
            blockRep = 0,
            perfRep = 0,
            newRep = 0
          )
        )

      val hotUpdater = mock[Set[RemotePeer] => F[Unit]]
      (hotUpdater.apply _)
        .expects(Set(RemotePeer(host1Id, host1Ra), RemotePeer(host3Id, host3Ra)))
        .once()
        .returns(().pure[F])

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          defaultBodyStorage,
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
            _ = assert(withUpdate.peersHandler(host1Id).state == PeerState.Hot)
            _ = assert(withUpdate.peersHandler(host1Id).closedTimestamps == Seq(1))
            _ = assert(withUpdate.peersHandler(host1Id).actorOpt.isDefined)
            _ = assert(withUpdate.peersHandler(host2Id).state == PeerState.Hot)
            _ = assert(withUpdate.peersHandler(host2Id).closedTimestamps == Seq(2))
            _ = assert(withUpdate.peersHandler(host2Id).actorOpt.isDefined)
            _ = assert(withUpdate.peersHandler(host3Id).state == PeerState.Hot)
            _ = assert(withUpdate.peersHandler(host3Id).closedTimestamps == Seq(3))
            _ = assert(withUpdate.peersHandler(host3Id).actorOpt.isDefined)
            _ = assert(withUpdate.peersHandler(host4Id).state == PeerState.Hot)
            _ = assert(withUpdate.peersHandler(host4Id).closedTimestamps == Seq(4))
            _ = assert(withUpdate.peersHandler(host4Id).actorOpt.isDefined)
            _ = assert(withUpdate.peersHandler(host5Id).state == PeerState.Cold)
            _ = assert(withUpdate.peersHandler(host5Id).closedTimestamps.size == 2)
            _ = assert(withUpdate.peersHandler(host5Id).actorOpt.isEmpty)
            _ = assert(withUpdate.peersHandler(host6Id).state == PeerState.Cold)
            _ = assert(withUpdate.peersHandler(host6Id).closedTimestamps.size == 2)
            _ = assert(withUpdate.peersHandler(host6Id).actorOpt.isDefined)
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
      val host1Id = arbitraryHost.arbitrary.first
      val host1Ra = RemoteAddress("first", 1)
      val peer1 = mockPeerActor[F]()
      val peer2 = mockPeerActor[F]()

      (networkAlgebra.makePeer _)
        .expects(host1Id, *, *, *, *, *, *, *, *, *, *, *, *, *, *)
        .once()
        .returns(
          Resource
            .pure(peer1)
            .onFinalize(Sync[F].delay(peer1.sendNoWait(PeerActor.Message.CloseConnection)))
        ) // simulate real actor finalizer
      (peer1.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = false, applicationLevel = false))
        .returns(Applicative[F].unit)
      (peer1.sendNoWait _)
        .expects(PeerActor.Message.GetNetworkQuality)
        .returns(Applicative[F].unit)
      (() => client1.remotePeerAsServer)
        .expects()
        .once()
        .returns(KnownHost(host1Id.id, host1Ra.host, host1Ra.port).some.pure[F])
      (peer1.sendNoWait _).expects(PeerActor.Message.CloseConnection).returns(Applicative[F].unit)

      (networkAlgebra.makePeer _)
        .expects(host1Id, *, *, *, *, *, *, *, *, *, *, *, *, *, *)
        .once()
        .returns(Resource.pure(peer2))
      (peer2.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = false, applicationLevel = false))
        .returns(Applicative[F].unit)
      (peer2.sendNoWait _)
        .expects(PeerActor.Message.GetNetworkQuality)
        .returns(Applicative[F].unit)
      val secondRemoteServerPort = 2
      (() => client2.remotePeerAsServer)
        .expects()
        .once()
        .returns(KnownHost(host1Id.id, host1Ra.host, secondRemoteServerPort).some.pure[F])

      val initialPeersMap: Map[HostId, Peer[F]] = Map.empty

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          defaultBodyStorage,
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
            _ <- actor.send(PeersManager.Message.SetupRequestsProxy(requestProxy))
            withUpdate <- actor.send(
              buildOpenedPeerConnectionMessage(client1, ConnectedPeer(host1Ra, host1Id.id))
            )
            _ = assert(withUpdate.peersHandler(host1Id).state == PeerState.Cold)
            _ = assert(withUpdate.peersHandler(host1Id).actorOpt.get == peer1)
            _ = assert(withUpdate.peersHandler(host1Id).asServer.get.address == host1Ra)
            _ = assert(withUpdate.peersHandler(host1Id).asServer.get.peerId == host1Id)
            withUpdate2 <- actor.send(PeersManager.Message.ClosePeer(host1Id))
            _ = assert(withUpdate2.peersHandler(host1Id).state == PeerState.Cold)
            _ = assert(withUpdate2.peersHandler(host1Id).actorOpt.isEmpty)
            withUpdate3 <- actor.send(
              buildOpenedPeerConnectionMessage(client2, ConnectedPeer(host1Ra, host1Id.id))
            )
            _ = assert(withUpdate3.peersHandler(host1Id).state == PeerState.Cold)
            _ = assert(withUpdate3.peersHandler(host1Id).actorOpt.get == peer2)
            _ = assert(withUpdate3.peersHandler(host1Id).asServer.get.address.port == secondRemoteServerPort)

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
      val host1Id = arbitraryHost.arbitrary.first
      val host1Ra = RemoteAddress("first", 1)
      val peer1 = mockPeerActor[F]()

      (networkAlgebra.makePeer _)
        .expects(host1Id, *, *, *, *, *, *, *, *, *, *, *, *, *, *)
        .once()
        .returns(
          Resource
            .pure(peer1)
            .onFinalize(Sync[F].delay(peer1.sendNoWait(PeerActor.Message.CloseConnection)))
        ) // simulate real actor finalizer
      (() => client1.remotePeerAsServer)
        .expects()
        .once()
        .returns(KnownHost(host1Id.id, host1Ra.host, host1Ra.port).some.pure[F])
      (peer1.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = false, applicationLevel = false))
        .returns(Applicative[F].unit)
      (peer1.sendNoWait _)
        .expects(PeerActor.Message.GetNetworkQuality)
        .returns(Applicative[F].unit)
      (peer1.sendNoWait _).expects(PeerActor.Message.CloseConnection).returns(Applicative[F].unit)

      val initialPeersMap: Map[HostId, Peer[F]] =
        Map(buildSimplePeerEntry(PeerState.Cold, None, host1Id, host1Ra, remoteNetworkLevel = false))

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          defaultBodyStorage,
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
            _ <- actor.send(PeersManager.Message.SetupRequestsProxy(requestProxy))
            withUpdate <- actor.send(
              buildOpenedPeerConnectionMessage(client1, ConnectedPeer(host1Ra, host1Id.id))
            )
            _ = assert(withUpdate.peersHandler(host1Id).state == PeerState.Cold)
            _ = assert(withUpdate.peersHandler(host1Id).asServer.get.address == host1Ra)
            _ = assert(withUpdate.peersHandler(host1Id).asServer.get.peerId == host1Id)
            _ = assert(withUpdate.peersHandler(host1Id).actorOpt.get == peer1)
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

      val host1Id = arbitraryHost.arbitrary.first
      val host1Ra = RemoteAddress("first", 1)
      val peer1 = mockPeerActor[F]()
      (peer1.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = true, applicationLevel = true))
        .returns(().pure[F])

      val host2Id = arbitraryHost.arbitrary.first
      val host2Ra = RemoteAddress("second", 2)
      val peer2 = mockPeerActor[F]()
      (peer2.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = true, applicationLevel = true))
        .returns(().pure[F])

      val host3Id = arbitraryHost.arbitrary.first
      val host3Ra = RemoteAddress("third", 3)
      val peer3 = mockPeerActor[F]()

      val host4Id = arbitraryHost.arbitrary.first
      val host4Ra = RemoteAddress("fourth", 4)
      val peer4 = mockPeerActor[F]()

      val initialPeersMap: Map[HostId, Peer[F]] =
        Map(
          buildPeerEntry(
            host1Id,
            PeerState.Warm,
            None,
            host1Ra.some,
            Option(peer1),
            closedTimestamps = Seq(1),
            perfRep = 1.0
          ),
          buildSimplePeerEntry(
            PeerState.Warm,
            Option(peer2),
            host2Id,
            host2Ra,
            closedTimestamps = Seq(2),
            perfRep = 0.9
          ),
          buildPeerEntry(
            host3Id,
            PeerState.Warm,
            None,
            host3Ra.some,
            Option(peer3),
            closedTimestamps = Seq(3),
            perfRep = 0.8
          ),
          buildPeerEntry(
            host4Id,
            PeerState.Warm,
            None,
            host4Ra.some,
            Option(peer4),
            closedTimestamps = Seq(4),
            perfRep = 0.7
          )
        )

      val hotUpdater = mock[Set[RemotePeer] => F[Unit]]
      (hotUpdater.apply _)
        .expects(Set(RemotePeer(host2Id, host2Ra)))
        .once()
        .returns(().pure[F]) // skip host1, because it have no server port

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          defaultBodyStorage,
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
            _ = assert(withUpdate.peersHandler(host1Id).state == PeerState.Hot)
            _ = assert(withUpdate.peersHandler(host1Id).closedTimestamps == Seq(1))
            _ = assert(withUpdate.peersHandler(host1Id).newRep == defaultP2PConfig.remotePeerNoveltyInSlots)
            _ = assert(withUpdate.peersHandler(host2Id).state == PeerState.Hot)
            _ = assert(withUpdate.peersHandler(host2Id).closedTimestamps == Seq(2))
            _ = assert(withUpdate.peersHandler(host2Id).newRep == defaultP2PConfig.remotePeerNoveltyInSlots)
            _ = assert(withUpdate.peersHandler(host3Id).state == PeerState.Warm)
            _ = assert(withUpdate.peersHandler(host3Id).closedTimestamps == Seq(3))
            _ = assert(withUpdate.peersHandler(host4Id).state == PeerState.Warm)
            _ = assert(withUpdate.peersHandler(host4Id).closedTimestamps == Seq(4))
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

      val host1Id = arbitraryHost.arbitrary.first
      val host1Ra = RemoteAddress("1", 1)
      val peer1 = mockPeerActor[F]()
      (networkAlgebra.makePeer _)
        .expects(host1Id, *, *, *, *, *, *, *, *, *, *, *, *, *, *)
        .once()
        .returns(Resource.pure(peer1))
      (peer1.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = false, applicationLevel = false))
        .returns(Applicative[F].unit)
      val client1 = mock[BlockchainPeerClient[F]]
      (() => client1.remotePeerAsServer)
        .expects()
        .once()
        .returns(KnownHost(host1Id.id, host1Ra.host, host1Ra.port).some.pure[F])
      (peer1.sendNoWait _)
        .expects(PeerActor.Message.GetNetworkQuality)
        .returns(Applicative[F].unit)

      val host2Id = arbitraryHost.arbitrary.first
      val host2Ra = RemoteAddress("2", 2)
      val peer2 = mockPeerActor[F]()
      (networkAlgebra.makePeer _)
        .expects(host2Id, *, *, *, *, *, *, *, *, *, *, *, *, *, *)
        .once()
        .returns(Resource.pure(peer2))
      (peer2.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = false, applicationLevel = false))
        .returns(Applicative[F].unit)
      val client2 = mock[BlockchainPeerClient[F]]
      (() => client2.remotePeerAsServer)
        .expects()
        .once()
        .returns(KnownHost(host2Id.id, host2Ra.host, host2Ra.port).some.pure[F])
      (peer2.sendNoWait _)
        .expects(PeerActor.Message.GetNetworkQuality)
        .returns(Applicative[F].unit)

      val host3Id = arbitraryHost.arbitrary.first
      val host3Ra = RemoteAddress("3", 3)
      val peer3 = mockPeerActor[F]()
      (networkAlgebra.makePeer _)
        .expects(host3Id, *, *, *, *, *, *, *, *, *, *, *, *, *, *)
        .once()
        .returns(Resource.pure(peer3))
      (peer3.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = true, applicationLevel = false))
        .returns(Applicative[F].unit)
      val client3 = mock[BlockchainPeerClient[F]]
      (() => client3.remotePeerAsServer)
        .expects()
        .once()
        .returns(KnownHost(host3Id.id, host3Ra.host, host3Ra.port).some.pure[F])
      (peer3.sendNoWait _)
        .expects(PeerActor.Message.GetNetworkQuality)
        .returns(Applicative[F].unit)

      val host4Id = arbitraryHost.arbitrary.first
      val host4Ra = RemoteAddress("4", 4)
      val client4 = mock[BlockchainPeerClient[F]]
      (client4.closeConnection _).expects().once().returns(().pure[F])

      val host5Id = arbitraryHost.arbitrary.first
      val host5Ra = RemoteAddress("5", 5)
      val peer5 = mockPeerActor[F]()
      (networkAlgebra.makePeer _)
        .expects(host5Id, *, *, *, *, *, *, *, *, *, *, *, *, *, *)
        .once()
        .returns(Resource.pure(peer5))
      (peer5.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = true, applicationLevel = true))
        .returns(Applicative[F].unit)
      val client5 = mock[BlockchainPeerClient[F]]
      val client5RemotePort = None
      (() => client5.remotePeerAsServer).expects().once().returns(client5RemotePort.pure[F])
      (peer5.sendNoWait _)
        .expects(PeerActor.Message.GetNetworkQuality)
        .returns(Applicative[F].unit)

      val host6Id = arbitraryHost.arbitrary.first
      val host6Ra = RemoteAddress("6", 6)
      val peer6 = mockPeerActor[F]()

      val host7Id = arbitraryHost.arbitrary.first
      val host7Ra = RemoteAddress("7", 7)
      val peer7 = mockPeerActor[F]()

      val initialPeersMap = Map(
        buildSimplePeerEntry(
          PeerState.Cold,
          None,
          host1Id,
          host1Ra,
          closedTimestamps = Seq(1),
          remoteNetworkLevel = false,
          noServer = true
        ),
        buildSimplePeerEntry(
          PeerState.Warm,
          None,
          host3Id,
          host3Ra,
          closedTimestamps = Seq(3),
          remoteNetworkLevel = true,
          noServer = true
        ),
        buildSimplePeerEntry(
          PeerState.Banned,
          None,
          host4Id,
          host4Ra,
          closedTimestamps = Seq(4),
          remoteNetworkLevel = true,
          noServer = true
        ),
        buildSimplePeerEntry(
          PeerState.Hot,
          None,
          host5Id,
          host5Ra,
          closedTimestamps = Seq(5),
          remoteNetworkLevel = true,
          noServer = true
        ),
        buildSimplePeerEntry(
          PeerState.Cold,
          Option(peer6),
          host6Id,
          host6Ra,
          closedTimestamps = Seq(6),
          remoteNetworkLevel = true,
          noServer = true
        ),
        buildSimplePeerEntry(
          PeerState.Warm,
          Option(peer7),
          host7Id,
          host7Ra,
          closedTimestamps = Seq(7),
          remoteNetworkLevel = true,
          noServer = true
        )
      )

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          defaultBodyStorage,
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
            stateHost1 <- actor.send(buildOpenedPeerConnectionMessage(client1, ConnectedPeer(host1Ra, host1Id.id)))
            _ = assert(stateHost1.peersHandler(host1Id).state == PeerState.Cold)
            _ = assert(stateHost1.peersHandler(host1Id).closedTimestamps == Seq(1))
            _ = assert(stateHost1.peersHandler(host1Id).asServer.get.address == host1Ra)
            _ = assert(stateHost1.peersHandler(host1Id).asServer.get.peerId == host1Id)
            stateHost2 <- actor.send(buildOpenedPeerConnectionMessage(client2, ConnectedPeer(host2Ra, host2Id.id)))
            _ = assert(stateHost2.peersHandler(host2Id).state == PeerState.Cold)
            _ = assert(stateHost2.peersHandler(host2Id).closedTimestamps == Seq.empty)
            _ = assert(stateHost2.peersHandler(host2Id).asServer.get.address == host2Ra)
            _ = assert(stateHost2.peersHandler(host2Id).asServer.get.peerId == host2Id)
            stateHost3 <- actor.send(buildOpenedPeerConnectionMessage(client3, ConnectedPeer(host3Ra, host3Id.id)))
            _ = assert(stateHost3.peersHandler(host3Id).state == PeerState.Warm)
            _ = assert(stateHost3.peersHandler(host3Id).closedTimestamps == Seq(3))
            _ = assert(stateHost3.peersHandler(host3Id).asServer.get.address == host3Ra)
            _ = assert(stateHost3.peersHandler(host3Id).asServer.get.peerId == host3Id)
            stateHost4 <- actor.send(buildOpenedPeerConnectionMessage(client4, ConnectedPeer(host4Ra, host4Id.id)))
            _ = assert(stateHost4.peersHandler(host4Id).state == PeerState.Banned)
            _ = assert(stateHost4.peersHandler(host4Id).closedTimestamps == Seq(4))
            _ = assert(stateHost4.peersHandler(host4Id).asServer.isEmpty)
            stateHost5 <- actor.send(buildOpenedPeerConnectionMessage(client5, ConnectedPeer(host5Ra, host5Id.id)))
            _ = assert(stateHost5.peersHandler(host5Id).state == PeerState.Hot)
            _ = assert(stateHost5.peersHandler(host5Id).closedTimestamps == Seq(5))
            _ = assert(stateHost5.peersHandler(host5Id).asServer == client5RemotePort)
            stateHost6 <- actor.send(
              buildOpenedPeerConnectionMessage(mock[BlockchainPeerClient[F]], ConnectedPeer(host6Ra, host6Id.id))
            )
            _ = assert(stateHost6.peersHandler(host6Id).state == PeerState.Cold)
            _ = assert(stateHost6.peersHandler(host6Id).closedTimestamps == Seq(6))
            _ = assert(stateHost6.peersHandler(host6Id).asServer.isEmpty)
            stateHost7 <- actor.send(
              buildOpenedPeerConnectionMessage(mock[BlockchainPeerClient[F]], ConnectedPeer(host7Ra, host7Id.id))
            )
            _ = assert(stateHost7.peersHandler(host7Id).state == PeerState.Warm)
            _ = assert(stateHost7.peersHandler(host7Id).closedTimestamps == Seq(7))
            _ = assert(stateHost7.peersHandler(host7Id).asServer.isEmpty)
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

      val host1Id = arbitraryHost.arbitrary.first
      val host1Ra = RemoteAddress("1", 1)
      val peer1 = mockPeerActor[F]()

      val host2Id = arbitraryHost.arbitrary.first
      val host2Ra = RemoteAddress("2", 2)
      val peer2 = mockPeerActor[F]()

      val host3Id = arbitraryHost.arbitrary.first
      val host3Ra = RemoteAddress("3", 3)
      val peer3 = mockPeerActor[F]()

      val host4Id = arbitraryHost.arbitrary.first
      val host4Ra = RemoteAddress("4", 4)
      val peer4 = mockPeerActor[F]()

      val initialPeersMap = Map(
        buildSimplePeerEntry(PeerState.Banned, Option(peer1), host1Id, host1Ra, closedTimestamps = Seq(1)),
        buildSimplePeerEntry(PeerState.Cold, Option(peer2), host2Id, host2Ra, closedTimestamps = Seq(3)),
        buildSimplePeerEntry(PeerState.Warm, Option(peer3), host3Id, host3Ra, closedTimestamps = Seq(4)),
        buildSimplePeerEntry(PeerState.Hot, Option(peer4), host4Id, host4Ra, closedTimestamps = Seq(5))
      )

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          defaultBodyStorage,
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
            stateHost1 <- actor.send(PeersManager.Message.RemotePeerNetworkLevel(host1Id, networkLevel = false))
            _ = assert(stateHost1.peersHandler(host1Id).state == PeerState.Banned)
            _ = assert(!stateHost1.peersHandler(host1Id).remoteNetworkLevel)
            _ = assert(stateHost1.peersHandler(host1Id).actorOpt.isEmpty)
            stateHost2 <- actor.send(PeersManager.Message.RemotePeerNetworkLevel(host2Id, networkLevel = false))
            _ = assert(stateHost2.peersHandler(host2Id).state == PeerState.Cold)
            _ = assert(!stateHost2.peersHandler(host2Id).remoteNetworkLevel)
            _ = assert(stateHost2.peersHandler(host2Id).actorOpt.isEmpty)
            stateHost3 <- actor.send(PeersManager.Message.RemotePeerNetworkLevel(host3Id, networkLevel = false))
            _ = assert(stateHost3.peersHandler(host3Id).state == PeerState.Warm)
            _ = assert(!stateHost3.peersHandler(host3Id).remoteNetworkLevel)
            _ = assert(stateHost3.peersHandler(host3Id).actorOpt.isDefined)
            stateHost4 <- actor.send(PeersManager.Message.RemotePeerNetworkLevel(host4Id, networkLevel = false))
            _ = assert(stateHost4.peersHandler(host4Id).state == PeerState.Hot)
            _ = assert(!stateHost4.peersHandler(host4Id).remoteNetworkLevel)
            _ = assert(stateHost4.peersHandler(host4Id).actorOpt.isDefined)
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

      val host1Id = arbitraryHost.arbitrary.first
      val host1 = "host1"
      val hostServer1Port = 1
      val host1Ra = RemoteAddress(host1, hostServer1Port)

      val host2Id = arbitraryHost.arbitrary.first
      val host2 = "host2"
      val hostServer2Port = 2
      val host2Ra = RemoteAddress(host2, hostServer2Port)

      val host3Id = arbitraryHost.arbitrary.first
      val host3 = "host3"
      val hostServer3Port = 3
      val host3Ra = RemoteAddress(host3, hostServer3Port)

      val host4Id = arbitraryHost.arbitrary.first
      val host4 = "host4"
      val hostServer4Port = 4
      val host4Ra = RemoteAddress(host4, hostServer4Port)

      val host5Id = arbitraryHost.arbitrary.first
      val host5 = "host5"
      val hostServer5Port = 5
      val host5Ra = RemoteAddress(host5, hostServer5Port)

      val host6Id = arbitraryHost.arbitrary.first
      val host6 = "host6"
      val hostServer6Port = 6
      val host6Ra = RemoteAddress(host6, hostServer6Port)

      val initialPeersMap: Map[HostId, Peer[F]] =
        Map(
          buildSimplePeerEntry(PeerState.Banned, None, host1Id, host1Ra),
          buildSimplePeerEntry(PeerState.Cold, None, host2Id, host2Ra),
          buildSimplePeerEntry(PeerState.Warm, None, host3Id, host3Ra),
          buildSimplePeerEntry(PeerState.Warm, None, host4Id, host4Ra),
          buildSimplePeerEntry(PeerState.Hot, None, host5Id, host5Ra),
          buildSimplePeerEntry(PeerState.Hot, None, host6Id, host6Ra, noServer = true)
        )

      val writingHosts = mock[Set[KnownRemotePeer] => F[Unit]]
      val expectedHosts: Set[KnownRemotePeer] = Set(
        KnownRemotePeer(host2Id, RemoteAddress(host2.reverse, hostServer2Port), 0, 0),
        KnownRemotePeer(host3Id, RemoteAddress(host3.reverse, hostServer3Port), 0, 0),
        KnownRemotePeer(host4Id, RemoteAddress(host4.reverse, hostServer4Port), 0, 0),
        KnownRemotePeer(host5Id, RemoteAddress(host5.reverse, hostServer5Port), 0, 0)
      )
      (writingHosts.apply _).expects(expectedHosts).once().returns(().pure[F])

      implicit val dummyReverseReverseDns: ReverseDnsResolver[F] = (h: String) => h.reverse.pure[F]
      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          defaultBodyStorage,
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
        )(implicitly[Async[IO]], implicitly[Parallel[IO]], logger, dummyDns, dummyReverseReverseDns)
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

      val remoteAddresses: NonEmptyChain[RemotePeer] =
        NonEmptyChain
          .fromSeq(
            (externalIPs ++ specialIPs).map(ip => RemotePeer(arbitraryHost.arbitrary.first, RemoteAddress(ip, 0))).toSeq
          )
          .get
      val peersToAdd: NonEmptyChain[KnownRemotePeer] =
        remoteAddresses.map(rp => KnownRemotePeer(rp.peerId, rp.address, 0, 0))

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          defaultBodyStorage,
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
            knownPeers1 = updatedState.peersHandler.peers.values.flatMap(_.asServer.map(_.address.host)).toSet
            _ = assert(externalIPs.forall(knownPeers1.contains))
            _ = assert(specialIPs.forall(!knownPeers1.contains(_)))
            updateWithAddKnown <- actor.send(PeersManager.Message.AddKnownPeers(peersToAdd))
            knownPeers2 = updateWithAddKnown.peersHandler.peers.values.flatMap(_.asServer.map(_.address.host)).toSet
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

      val address1Id = arbitraryHost.arbitrary.first
      val address1 = RemoteAddress("10.0.0.1", 1)
      val address2Id = arbitraryHost.arbitrary.first
      val address2 = RemoteAddress("10.0.0.2", 2)

      val host1Id = arbitraryHost.arbitrary.first
      val host1 = "1"
      val hostServer1Port = 1
      val host1Ra = RemoteAddress(host1, hostServer1Port)
      val host1BlockRep = 0.6
      val host1PerfRep = 0.8

      val initialPeersMap: Map[HostId, Peer[F]] =
        Map(
          buildSimplePeerEntry(PeerState.Hot, None, host1Id, host1Ra, blockRep = host1BlockRep, perfRep = host1PerfRep)
        )

      val neighbours = NonEmptyChain(RemotePeer(address1Id, address1), RemotePeer(address2Id, address2))

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          defaultBodyStorage,
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
            updatedState <- actor.send(PeersManager.Message.AddKnownNeighbors(host1Id, neighbours))
            knownPeers1 = updatedState.peersHandler.peers
            _ = assert(knownPeers1(address1Id).blockRep == host1BlockRep)
            _ = assert(knownPeers1(address1Id).perfRep == 0.0)
            _ = assert(knownPeers1(address2Id).blockRep == host1BlockRep)
            _ = assert(knownPeers1(address2Id).perfRep == 0.0)
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
        buildSimplePeerEntry(PeerState.Hot, Option(peer), blockSource),
        buildSimplePeerEntry(PeerState.Cold, Option(mockPeerActor[F]()), coldPeer),
        buildSimplePeerEntry(PeerState.Warm, Option(mockPeerActor[F]()), preWarmPeer),
        buildSimplePeerEntry(PeerState.Banned, Option(mockPeerActor[F]()), bannedPeer),
        buildSimplePeerEntry(PeerState.Warm, Option(mockPeerActor[F]()), warmPeer)
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
          defaultBodyStorage,
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
      val initialPeersMap: Map[HostId, Peer[F]] = Map(buildSimplePeerEntry(PeerState.Hot, Option(peer), blockSource))

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
          defaultBodyStorage,
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
        buildSimplePeerEntry(PeerState.Cold, Option(mockPeerActor[F]()), coldPeer),
        buildSimplePeerEntry(PeerState.Warm, Option(mockPeerActor[F]()), preWarmPeer),
        buildSimplePeerEntry(PeerState.Banned, Option(mockPeerActor[F]()), bannedPeer),
        buildSimplePeerEntry(PeerState.Warm, Option(mockPeerActor[F]()), warmPeer)
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
          defaultBodyStorage,
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
        buildSimplePeerEntry(PeerState.Hot, Option(peer), blockSource),
        buildSimplePeerEntry(PeerState.Cold, Option(mockPeerActor[F]()), coldPeer),
        buildSimplePeerEntry(PeerState.Warm, Option(mockPeerActor[F]()), preWarmPeer),
        buildSimplePeerEntry(PeerState.Banned, Option(mockPeerActor[F]()), bannedPeer),
        buildSimplePeerEntry(PeerState.Warm, Option(mockPeerActor[F]()), warmPeer)
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
          defaultBodyStorage,
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
      val initialPeersMap: Map[HostId, Peer[F]] = Map(buildSimplePeerEntry(PeerState.Hot, Option(peer), blockSource))

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
          defaultBodyStorage,
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
        buildSimplePeerEntry(PeerState.Cold, Option(mockPeerActor[F]()), coldPeer),
        buildSimplePeerEntry(PeerState.Warm, Option(mockPeerActor[F]()), preWarmPeer),
        buildSimplePeerEntry(PeerState.Banned, Option(mockPeerActor[F]()), bannedPeer),
        buildSimplePeerEntry(PeerState.Warm, Option(mockPeerActor[F]()), warmPeer)
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
          defaultBodyStorage,
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
        buildSimplePeerEntry(
          PeerState.Cold,
          Option(mockPeerActor[F]()),
          hostId,
          blockRep = initBlock,
          perfRep = 0.5,
          newRep = initNovelty
        )
      )

      val delay = 230L
      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          defaultBodyStorage,
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
        buildSimplePeerEntry(
          PeerState.Cold,
          Option(mockPeerActor[F]()),
          hostId,
          blockRep = initBlock,
          perfRep = 0.5,
          newRep = initNovelty
        )
      )

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          defaultBodyStorage,
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
        buildSimplePeerEntry(
          PeerState.Cold,
          Option(mockPeerActor[F]()),
          hostId,
          blockRep = initBlock,
          perfRep = 0.5,
          newRep = initNovelty
        )
      )

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          defaultBodyStorage,
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
        buildSimplePeerEntry(
          PeerState.Cold,
          Option(mockPeerActor[F]()),
          hostId,
          blockRep = initBlock,
          perfRep = 0.5,
          newRep = initNovelty
        )
      )

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          defaultBodyStorage,
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
        buildSimplePeerEntry(
          PeerState.Cold,
          Option(mockPeerActor[F]()),
          hostId,
          blockRep = initBlock,
          perfRep = initPerf,
          newRep = initNovelty
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
          defaultBodyStorage,
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
          buildSimplePeerEntry(
            PeerState.Cold,
            Option(mockPeerActor[F]()),
            hostId,
            blockRep = initBlock,
            perfRep = initPerf,
            newRep = initNovelty
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
            defaultBodyStorage,
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
        buildSimplePeerEntry(PeerState.Hot, Option(mockPeerActor[F]()), host1, blockRep = worstReputation),
        buildSimplePeerEntry(PeerState.Hot, Option(mockPeerActor[F]()), host2, blockRep = worstReputation),
        buildSimplePeerEntry(PeerState.Hot, Option(mockPeerActor[F]()), host3, blockRep = worstReputation),
        buildSimplePeerEntry(PeerState.Hot, Option(mockPeerActor[F]()), host4, blockRep = worstReputation),
        buildSimplePeerEntry(PeerState.Hot, Option(mockPeerActor[F]()), host5, blockRep = worstedReputation)
      )

      val cache = Caffeine.newBuilder.maximumSize(blockSourceCacheSize).build[BlockId, Set[HostId]]()
      cache.put(blockId5, Set(host1, host2, host3))

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          defaultBodyStorage,
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
      val initialPeersMap: Map[HostId, Peer[F]] = Map(buildSimplePeerEntry(PeerState.Hot, Option(peerActor), hostId))

      (peerActor.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = false, applicationLevel = false))
        .returns(Applicative[F].unit)

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          defaultBodyStorage,
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

  test("Cold peers and close timestamps shall be cleared") {
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

      val host1Id = arbitraryHost.arbitrary.first
      val host1 = "1"
      val hostServer1Port = 2
      val host1Ra = RemoteAddress(host1, hostServer1Port)

      val host2Id = arbitraryHost.arbitrary.first
      val host2 = "2"
      val hostServer2Port = 2
      val host2Ra = RemoteAddress(host2, hostServer2Port)

      val host3Id = arbitraryHost.arbitrary.first
      val host3 = "3"
      val hostServer3Port = 3
      val host3Ra = RemoteAddress(host3, hostServer3Port)

      val host4Id = arbitraryHost.arbitrary.first
      val host4 = "4"
      val hostServer4Port = 4
      val host4Ra = RemoteAddress(host4, hostServer4Port)

      val host5Id = arbitraryHost.arbitrary.first
      val host5 = "5"
      val hostServer5Port = 5
      val host5Ra = RemoteAddress(host5, hostServer5Port)

      val host6Id = arbitraryHost.arbitrary.first
      val host6 = "6"
      val hostServer6Port = 6
      val host6Ra = RemoteAddress(host6, hostServer6Port)

      val initialPeersMap: Map[HostId, Peer[F]] = Map(
        buildSimplePeerEntry(PeerState.Cold, None, host1Id, host1Ra, closedTimestamps = Seq.empty, noServer = true),
        buildSimplePeerEntry(PeerState.Cold, None, host2Id, host2Ra, closedTimestamps = Seq(0)),
        buildSimplePeerEntry(PeerState.Cold, None, host3Id, host3Ra, closedTimestamps = Seq(0, 1)),
        buildSimplePeerEntry(PeerState.Cold, None, host4Id, host4Ra, closedTimestamps = Seq(0, 1)),
        buildSimplePeerEntry(
          PeerState.Cold,
          None,
          host5Id,
          host5Ra,
          closedTimestamps = Seq(System.currentTimeMillis())
        ),
        buildSimplePeerEntry(PeerState.Cold, None, host6Id, host6Ra, closedTimestamps = Seq(System.currentTimeMillis()))
      )

      val networkConfig =
        defaultP2PConfig.networkProperties.copy(
          maximumEligibleColdConnections = 2,
          minimumEligibleColdConnections = 1,
          closeTimeoutWindowInMs = 20000,
          closeTimeoutFirstDelayInMs = 1000000
        )

      val config = defaultP2PConfig.copy(networkProperties = networkConfig)
      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          defaultBodyStorage,
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
            newState <- actor.send(PeersManager.Message.UpdatePeersTick)
            _ = assert(newState.peersHandler.getColdPeers.size == 4) // 1 saved + non eligible 3 cold peers
            _ = assert(newState.peersHandler.getColdPeers.contains(host2Id))
            _ = assert(newState.peersHandler(host2Id).closedTimestamps.isEmpty)
            _ = assert(newState.peersHandler.getColdPeers.contains(host1Id))
            _ = assert(newState.peersHandler(host1Id).closedTimestamps.isEmpty)
            _ = assert(newState.peersHandler.getColdPeers.contains(host5Id))
            _ = assert(newState.peersHandler(host5Id).closedTimestamps.size == 1)
            _ = assert(newState.peersHandler.getColdPeers.contains(host6Id))
            _ = assert(newState.peersHandler(host6Id).closedTimestamps.size == 1)
          } yield ()
        }
    }
  }

  test("Updated remote node id shall be processed: normal id change") {
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

      val host1IdOld = arbitraryHost.arbitrary.first
      val host1IdNew = arbitraryHost.arbitrary.first
      val host1 = "1"
      val host1Peer = Peer(
        PeerState.Warm,
        None,
        RemoteAddress(host1, 1).some,
        None,
        Seq.empty,
        remoteNetworkLevel = true,
        0,
        0.0,
        0
      )
      val host2Id = arbitraryHost.arbitrary.first
      val host2 = "2"

      val initialPeersMap: Map[HostId, Peer[F]] = Map(
        host1IdOld -> host1Peer,
        host2Id -> Peer(
          PeerState.Cold,
          None,
          RemoteAddress(host2, 2).some,
          asServer(RemoteAddress(host2, 2)),
          Seq(0),
          remoteNetworkLevel = true,
          0,
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
          defaultBodyStorage,
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
            newState <- actor.send(PeersManager.Message.RemotePeerIdChanged(host1IdOld, host1IdNew))
            _ = assert(newState.peersHandler.peers.size == initialPeersMap.size + 1) // +1 because of self-banned peer
            _ = assert(newState.peersHandler.getColdPeers.contains(host2Id))
            _ = assert(!newState.peersHandler.peers.contains(host1IdOld))
            _ = assert(newState.peersHandler.peers(host1IdNew) == host1Peer)
          } yield ()
        }
    }
  }

  test("Updated remote node id shall be processed: close peer if try to update the peer id to already exist peer") {
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

      val host1Id = arbitraryHost.arbitrary.first
      val host1Ra = RemoteAddress("1", 1)
      val client1 = mock[BlockchainPeerClient[F]]
      val host2Id = arbitraryHost.arbitrary.first
      val host2PeerActor = mockPeerActor[F]()
      val peer1 = mockPeerActor[F]()

      (networkAlgebra.makePeer _)
        .expects(host1Id, *, client1, *, *, *, *, *, *, *, *, *, *, *, *)
        .once()
        .returns(
          Resource
            .pure(peer1)
            .onFinalize(Sync[F].delay(peer1.sendNoWait(PeerActor.Message.CloseConnection)))
        ) // simulate real actor finalizer

      (peer1.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = false, applicationLevel = false))
        .returning(Applicative[F].unit)
      (peer1.sendNoWait _).expects(PeerActor.Message.GetNetworkQuality).returning(Applicative[F].unit)
      (peer1.sendNoWait _).expects(PeerActor.Message.CloseConnection).returning(Applicative[F].unit)

      val initialPeersMap: Map[HostId, Peer[F]] = Map(
        buildSimplePeerEntry(PeerState.Hot, host2PeerActor.some, host2Id)
      )

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          defaultBodyStorage,
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
            _ <- actor.send(PeersManager.Message.SetupRequestsProxy(mock[RequestsProxyActor[F]]))
            newState1 <-
              actor.send(buildOpenedPeerConnectionMessage(client1, ConnectedPeer(host1Ra, host1Id.id)))
            _ = assert(newState1.peersHandler.peers(host1Id).state == PeerState.Cold)

            newState2 <- actor.send(PeersManager.Message.RemotePeerIdChanged(host2Id, host1Id))
            _ = assert(newState2.peersHandler.peers.size == (1 + 1)) // +1 because of self-banned peer
            _ = assert(newState2.peersHandler.getHotPeers.contains(host1Id))
            _ = assert(newState2.peersHandler.peers(host1Id).actorOpt == host2PeerActor.some)
            _ = assert(!newState2.peersHandler.peers.contains(host2Id))
          } yield ()
        }
    }
  }

  test("PeerManager shall self-ban own peer id to avoid self-connections") {
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

      PeersManager
        .makeActor(
          thisHostId,
          networkAlgebra,
          localChain,
          slotDataStore,
          defaultBodyStorage,
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
            newState <- actor.send(PeersManager.Message.SetupBlockChecker(mock[BlockCheckerActor[F]]))
            _ = assert(newState.peersHandler.peers(thisHostId).state == PeerState.Banned)
          } yield ()
        }
    }
  }
}
