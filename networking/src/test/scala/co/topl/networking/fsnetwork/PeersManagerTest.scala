package co.topl.networking.fsnetwork

import cats.data.NonEmptyChain
import cats.effect.kernel.Sync
import cats.effect.{Async, IO, Resource}
import cats.implicits._
import cats.{Applicative, Parallel}
import co.topl.algebras.Store
import co.topl.brambl.generators.TransactionGenerator
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.validation.algebras.TransactionSyntaxVerifier
import co.topl.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import co.topl.config.ApplicationConfig.Bifrost.NetworkProperties
import co.topl.consensus.algebras.{BlockHeaderToBodyValidationAlgebra, ChainSelectionAlgebra, LocalChainAlgebra}
import co.topl.consensus.models.{BlockId, SlotData}
import co.topl.eventtree.ParentChildTree
import co.topl.ledger.algebras.MempoolAlgebra
import co.topl.models.ModelGenerators.GenHelper
import co.topl.models.generators.consensus.ModelGenerators
import co.topl.models.generators.consensus.ModelGenerators.arbitraryBlockId
import co.topl.models.p2p._
import co.topl.networking.blockchain.{BlockchainPeerClient, NetworkProtocolVersions}
import co.topl.networking.fsnetwork.BlockChecker.BlockCheckerActor
import co.topl.networking.fsnetwork.NetworkQualityError.{IncorrectPongMessage, NoPongMessage}
import co.topl.networking.fsnetwork.PeerActor.PeerActor
import co.topl.networking.fsnetwork.PeersManager.PeersManagerActor
import co.topl.networking.fsnetwork.PeersManagerTest.F
import co.topl.networking.fsnetwork.RequestsProxy.RequestsProxyActor
import co.topl.networking.fsnetwork.TestHelper.{arbitraryHost, arbitraryRemoteAddress}
import co.topl.networking.p2p.{ConnectedPeer, DisconnectedPeer}
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
import co.topl.algebras.Stats

object PeersManagerTest {
  type F[A] = IO[A]
}

class PeersManagerTest
    extends CatsEffectSuite
    with ScalaCheckEffectSuite
    with AsyncMockFactory
    with TransactionGenerator {
  implicit val logger: Logger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)
  implicit val stats: Stats[F] = Stats.noop[F]
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

  def buildOpenedPeerConnectionMessage[F[_]](
    client:        BlockchainPeerClient[F],
    connectedPeer: ConnectedPeer,
    remoteServer:  Boolean = true
  ): PeersManager.Message.OpenedPeerConnection[F] = {
    (() => client.remotePeer).expects().anyNumberOfTimes().returns(connectedPeer)

    val asServer =
      if (remoteServer)
        KnownHost(connectedPeer.p2pVK, connectedPeer.remoteAddress.host, connectedPeer.remoteAddress.port).some
      else None

    PeersManager.Message.OpenedPeerConnection(client, asServer)
  }

  def buildSimplePeerEntry(
    state:               PeerState,
    actorOpt:            Option[PeerActor[F]] = None,
    id:                  HostId = arbitraryHost.arbitrary.first,
    address:             RemoteAddress = arbitraryRemoteAddress.arbitrary.first,
    closedTimestamps:    Seq[Long] = Seq.empty,
    remoteNetworkLevel:  Boolean = true,
    blockRep:            HostReputationValue = 0,
    perfRep:             HostReputationValue = 0,
    newRep:              Long = 0,
    noServer:            Boolean = false,
    connectionTimestamp: Option[Long] = None
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
    newRep,
    connectionTimestamp
  )

  def buildPeerEntry(
    id:                  HostId,
    state:               PeerState,
    serverAddress:       Option[RemoteAddress],
    connectedAddress:    Option[RemoteAddress],
    actorOpt:            Option[PeerActor[F]] = None,
    closedTimestamps:    Seq[Long] = Seq.empty,
    remoteNetworkLevel:  Boolean = true,
    blockRep:            HostReputationValue = 0,
    perfRep:             HostReputationValue = 0,
    newRep:              Long = 0,
    connectionTimestamp: Option[Long] = None
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
      newRep,
      connectionTimestamp
    )

  case class PeerManagerMockData(
    thisHostId:                  HostId,
    networkAlgebra:              NetworkAlgebra[F],
    localChain:                  LocalChainAlgebra[F],
    chainSelection:              ChainSelectionAlgebra[F, SlotData],
    slotDataStore:               Store[F, BlockId, SlotData],
    bodyStore:                   Store[F, BlockId, BlockBody],
    transactionStore:            Store[F, TransactionId, IoTransaction],
    blockIdTree:                 ParentChildTree[F, BlockId],
    mempool:                     MempoolAlgebra[F],
    headerToBodyValidation:      BlockHeaderToBodyValidationAlgebra[F],
    transactionSyntaxValidation: TransactionSyntaxVerifier[F],
    newPeerCreationAlgebra:      PeerCreationRequestAlgebra[F],
    p2pConfig:                   P2PNetworkConfig,
    hotPeersUpdate:              Set[RemotePeer] => F[Unit],
    savePeersFunction:           Set[KnownRemotePeer] => F[Unit],
    coldToWarmSelector:          SelectorColdToWarm[F],
    warmToHotSelector:           SelectorWarmToHot[F],
    initialPeers:                Map[HostId, Peer[F]],
    blockSource:                 Cache[BlockId, Set[HostId]]
  )

  private def buildDefaultMockData(
    networkAlgebra:         NetworkAlgebra[F] = mock[NetworkAlgebra[F]],
    p2pConfig:              P2PNetworkConfig = defaultP2PConfig,
    hotPeersUpdate:         Set[RemotePeer] => F[Unit] = defaultHotPeerUpdater,
    savePeersFunction:      Set[KnownRemotePeer] => F[Unit] = defaultPeersSaver,
    coldToWarmSelector:     SelectorColdToWarm[F] = defaultColdToWarmSelector,
    warmToHotSelector:      SelectorWarmToHot[F] = defaultWarmToHotSelector,
    initialPeers:           Map[HostId, Peer[F]] = Map.empty,
    newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]],
    blockSource:            Cache[BlockId, Set[HostId]] = defaultCache()
  ): PeerManagerMockData =
    PeerManagerMockData(
      thisHostId,
      networkAlgebra = networkAlgebra,
      localChain = mock[LocalChainAlgebra[F]],
      chainSelection = mock[ChainSelectionAlgebra[F, SlotData]],
      slotDataStore = mock[Store[F, BlockId, SlotData]],
      bodyStore = defaultBodyStorage,
      transactionStore = mock[Store[F, TransactionId, IoTransaction]],
      blockIdTree = mock[ParentChildTree[F, BlockId]],
      mempool = mock[MempoolAlgebra[F]],
      headerToBodyValidation = mock[BlockHeaderToBodyValidationAlgebra[F]],
      transactionSyntaxValidation = defaultTransactionSyntaxValidation,
      newPeerCreationAlgebra = newPeerCreationAlgebra,
      p2pConfig = p2pConfig,
      hotPeersUpdate = hotPeersUpdate,
      savePeersFunction = savePeersFunction,
      coldToWarmSelector = coldToWarmSelector,
      warmToHotSelector = warmToHotSelector,
      initialPeers = initialPeers,
      blockSource = blockSource
    )

  private def buildActorFromMockData(
    mockData:           PeerManagerMockData,
    dnsResolver:        DnsResolver[F] = implicitly[DnsResolver[F]],
    reverseDnsResolver: ReverseDnsResolver[F] = implicitly[ReverseDnsResolver[F]]
  ): Resource[F, PeersManagerActor[F]] =
    PeersManager
      .makeActor(
        mockData.thisHostId,
        mockData.networkAlgebra,
        mockData.localChain,
        mockData.chainSelection,
        mockData.slotDataStore,
        mockData.bodyStore,
        mockData.transactionStore,
        mockData.blockIdTree,
        mockData.mempool,
        mockData.headerToBodyValidation,
        mockData.transactionSyntaxValidation,
        mockData.newPeerCreationAlgebra,
        mockData.p2pConfig,
        mockData.hotPeersUpdate,
        mockData.savePeersFunction,
        mockData.coldToWarmSelector,
        mockData.warmToHotSelector,
        mockData.initialPeers,
        mockData.blockSource
      )(implicitly[Async[IO]], implicitly[Parallel[IO]], logger, dnsResolver, reverseDnsResolver, stats)

  test("Get current tips request shall be forwarded if application level is enabled") {
    withMock {
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

      val mockData = buildDefaultMockData(initialPeers = initialPeersMap)
      buildActorFromMockData(mockData)
        .use { actor =>
          for {
            _ <- actor.send(PeersManager.Message.GetCurrentTips)
          } yield ()
        }
    }
  }

  test("Get network quality shall be forwarded to warm hosts") {
    withMock {
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

      val mockData = buildDefaultMockData(initialPeers = initialPeersMap)
      buildActorFromMockData(mockData)
        .use { actor =>
          for {
            _ <- actor.send(PeersManager.Message.GetNetworkQualityForWarmHosts)
          } yield ()
        }
    }
  }

  test("Track common ancestor shall be forwarded to all connections") {
    withMock {
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
      val mockData = buildDefaultMockData(initialPeers = initialPeersMap)
      import co.topl.models.generators.consensus.ModelGenerators.arbitrarySlotData
      (() => mockData.localChain.head).expects().once().returns(arbitrarySlotData.arbitrary.first.pure[F])

      val requestsProxy = mock[RequestsProxyActor[F]]
      (() => requestsProxy.mailboxSize()).expects().once().returns(0.pure[F])

      val blockChecker = mock[BlockCheckerActor[F]]
      (() => blockChecker.mailboxSize()).expects().once().returns(0.pure[F])

      buildActorFromMockData(mockData).use { actor =>
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

      val mockData = buildDefaultMockData(initialPeers = initialPeersMap)
      buildActorFromMockData(mockData)
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

      val mockData = buildDefaultMockData(initialPeers = initialPeersMap)
      buildActorFromMockData(mockData)
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

      val mockData = buildDefaultMockData(initialPeers = initialPeersMap)
      buildActorFromMockData(mockData)
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
      val peerActor = mockPeerActor[F]()
      (peerActor.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = false, applicationLevel = false))
        .returns(().pure[F])

      val timeoutWindows = 1000
      val initialPeersMap =
        Map.empty[HostId, Peer[F]] + buildSimplePeerEntry(
          PeerState.Hot,
          Option(peerActor),
          hostId,
          closedTimestamps = Seq(0, 200, System.currentTimeMillis() - timeoutWindows - 1, System.currentTimeMillis())
        )

      val mockData = buildDefaultMockData(initialPeers = initialPeersMap)
      buildActorFromMockData(mockData)
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

      val mockData = buildDefaultMockData(p2pConfig = p2pConfig)
      (mockData.newPeerCreationAlgebra.requestNewPeerCreation _).expects(host1).returns(().pure[F])
      (mockData.newPeerCreationAlgebra.requestNewPeerCreation _).expects(host2).returns(().pure[F])
      buildActorFromMockData(mockData)
        .use { actor =>
          val peers =
            NonEmptyChain(
              KnownRemotePeer(host1Id, host1Ra, 0, 0, None),
              KnownRemotePeer(host2Id, host2Ra, 0, 0, None),
              KnownRemotePeer(host3Id, host3Ra, 0, 0, None)
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
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]

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

      val mockData = buildDefaultMockData(
        p2pConfig = p2pConfig,
        initialPeers = initialPeersMap,
        newPeerCreationAlgebra = newPeerCreationAlgebra
      )
      buildActorFromMockData(mockData)
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
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]

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

      val mockData = buildDefaultMockData(
        p2pConfig = p2pConfig,
        initialPeers = initialPeersMap,
        newPeerCreationAlgebra = newPeerCreationAlgebra
      )
      buildActorFromMockData(mockData)
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

      val mockData = buildDefaultMockData(p2pConfig = p2pConfig, initialPeers = initialPeersMap)
      buildActorFromMockData(mockData)
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

  test("Aggressive P2P: move some warm peers to hot if number of closed connections is ok") {
    withMock {
      val p2pConfig: P2PNetworkConfig =
        defaultP2PConfig.copy(networkProperties =
          NetworkProperties(aggressiveP2PCount = 1, aggressiveP2PMaxCloseEvent = 1)
        )

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
          buildSimplePeerEntry(
            PeerState.Warm,
            peer2.some,
            host2Id,
            host2Ra,
            blockRep = 1.0,
            closedTimestamps = Seq(0, 1, 2)
          ),
          buildSimplePeerEntry(PeerState.Warm, None, host3Id, host3Ra, blockRep = 0.1)
        )

      val mockData = buildDefaultMockData(p2pConfig = p2pConfig, initialPeers = initialPeersMap)
      buildActorFromMockData(mockData)
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

      val mockData = buildDefaultMockData(p2pConfig = p2pConfig, initialPeers = initialPeersMap)
      buildActorFromMockData(mockData)
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
      val newPeerCreationAlgebra: PeerCreationRequestAlgebra[F] = mock[PeerCreationRequestAlgebra[F]]
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

      val mockData = buildDefaultMockData(
        p2pConfig = p2pConfig,
        newPeerCreationAlgebra = newPeerCreationAlgebra,
        initialPeers = initialPeersMap
      )
      buildActorFromMockData(mockData)
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

  test("Reputation update: Do not move non-eligible by timeout and to warm because at least one warm peer") {
    withMock {
      val host1Id = arbitraryHost.arbitrary.first
      val host1Ra = RemoteAddress("1", 1)
      val host2Id = arbitraryHost.arbitrary.first
      val host2Ra = RemoteAddress("2", 2)
      val host3Id = arbitraryHost.arbitrary.first
      val host3Ra = RemoteAddress("3", 3)
      val host4Id = arbitraryHost.arbitrary.first
      val host4Ra = RemoteAddress("4", 4)
      val host4Actor = mock[PeerActor[F]]
      (host4Actor.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = true, applicationLevel = true))
        .returning(Applicative[F].unit)

      val initialPeersMap: Map[HostId, Peer[F]] =
        Map(
          buildSimplePeerEntry(
            PeerState.Cold,
            None,
            host1Id,
            host1Ra,
            closedTimestamps = Seq(System.currentTimeMillis())
          ),
          buildSimplePeerEntry(
            PeerState.Cold,
            None,
            host2Id,
            host2Ra,
            closedTimestamps = Seq(System.currentTimeMillis(), System.currentTimeMillis())
          ),
          buildSimplePeerEntry(
            PeerState.Cold,
            None,
            host3Id,
            host3Ra,
            closedTimestamps = Seq(System.currentTimeMillis(), System.currentTimeMillis(), System.currentTimeMillis())
          ),
          buildSimplePeerEntry(PeerState.Warm, host4Actor.some, host4Id, host4Ra)
        )

      val mockData = buildDefaultMockData(initialPeers = initialPeersMap)
      buildActorFromMockData(mockData)
        .use { actor =>
          for {
            withUpdate <- actor.send(PeersManager.Message.UpdatedReputationTick)
            _ = assert(withUpdate.peersHandler(host1Id).state == PeerState.Cold)
            _ = assert(withUpdate.peersHandler(host2Id).state == PeerState.Cold)
            _ = assert(withUpdate.peersHandler(host3Id).state == PeerState.Cold)
            _ = assert(withUpdate.peersHandler(host4Id).state == PeerState.Hot)
          } yield ()
        }
    }
  }

  test("Adding cold peer: ignore banned peers") {
    withMock {
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

      val mockData = buildDefaultMockData(p2pConfig = p2pConfig, initialPeers = initialPeersMap)
      buildActorFromMockData(mockData)
        .use { actor =>
          val peers: NonEmptyChain[KnownRemotePeer] = NonEmptyChain(
            KnownRemotePeer(host1Id, host1Ra, 0, 0, None),
            KnownRemotePeer(host2Id, host2Ra, 0, 0, None),
            KnownRemotePeer(host3Id, host3Ra, 0, 0, None),
            KnownRemotePeer(host4Id, host4Ra, 0, 0, None)
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

      val minimumBlockProvidingReputation = defaultP2PConfig.networkProperties.minimumBlockProvidingReputation
      val initialPeersMap: Map[HostId, Peer[F]] =
        Map(
          buildSimplePeerEntry(
            PeerState.Hot,
            Option(peer1),
            host1Id,
            host1Ra,
            closedTimestamps = Seq(1),
            blockRep = minimumBlockProvidingReputation * defaultP2PConfig.blockNoveltyDecoy * 1.05,
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

      val mockData = buildDefaultMockData(hotPeersUpdate = hotUpdater, initialPeers = initialPeersMap)
      buildActorFromMockData(mockData)
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
      val requestProxy = mock[RequestsProxyActor[F]]

      val client1 = mock[BlockchainPeerClient[F]]
      val client2 = mock[BlockchainPeerClient[F]]
      val host1Id = arbitraryHost.arbitrary.first
      val host1Ra = RemoteAddress("first", 1)
      val host2Ra = RemoteAddress("second", 2)
      val peer1 = mockPeerActor[F]()
      val peer2 = mockPeerActor[F]()

      (networkAlgebra.makePeer _)
        .expects(host1Id, *, *, *, *, *, *, *, *, *, *, *, *, *, *)
        .once()
        .returns(
          Resource
            .pure(peer1)
            .onFinalize(Sync[F].delay(peer1.sendNoWait(PeerActor.Message.CloseConnectionForActor)))
        ) // simulate real actor finalizer
      (peer1.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = false, applicationLevel = false))
        .returns(Applicative[F].unit)
      (peer1.sendNoWait _)
        .expects(PeerActor.Message.GetNetworkQuality)
        .returns(Applicative[F].unit)
      (peer1.sendNoWait _).expects(PeerActor.Message.CloseConnectionForActor).returns(Applicative[F].unit)

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

      val mockData = buildDefaultMockData(networkAlgebra = networkAlgebra)
      buildActorFromMockData(mockData)
        .use { actor =>
          for {
            timeNow <- Sync[F].realTime.map(_.toMillis)
            _       <- actor.send(PeersManager.Message.SetupRequestsProxy(requestProxy))
            withUpdate <- actor.send(
              buildOpenedPeerConnectionMessage(client1, ConnectedPeer(host1Ra, host1Id.id, NetworkProtocolVersions.V1))
            )
            _ = assert(withUpdate.peersHandler(host1Id).state == PeerState.Cold)
            _ = assert(withUpdate.peersHandler(host1Id).actorOpt.get == peer1)
            _ = assert(withUpdate.peersHandler(host1Id).asServer.get.address == host1Ra)
            _ = assert(withUpdate.peersHandler(host1Id).asServer.get.peerId == host1Id)
            _ = assert(withUpdate.peersHandler(host1Id).activeConnectionTimestamp.isDefined)
            _ = assert(withUpdate.peersHandler(host1Id).activeConnectionTimestamp.get > timeNow)
            withUpdate2 <- actor.send(PeersManager.Message.ClosePeer(host1Id))
            _ = assert(withUpdate2.peersHandler(host1Id).state == PeerState.Cold)
            _ = assert(withUpdate2.peersHandler(host1Id).actorOpt.isEmpty)
            withUpdate3 <- actor.send(
              buildOpenedPeerConnectionMessage(client2, ConnectedPeer(host2Ra, host1Id.id, NetworkProtocolVersions.V1))
            )
            _ = assert(withUpdate3.peersHandler(host1Id).state == PeerState.Cold)
            _ = assert(withUpdate3.peersHandler(host1Id).actorOpt.get == peer2)
            _ = assert(withUpdate3.peersHandler(host1Id).asServer.get.address.host == host2Ra.host)
            _ = assert(withUpdate3.peersHandler(host1Id).asServer.get.address.port == host2Ra.port)

          } yield ()
        }
    }
  }

  test("Cold peer and then incoming new connection shall create new peer actor") {
    withMock {
      val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]
      val requestProxy = mock[RequestsProxyActor[F]]

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
            .onFinalize(Sync[F].delay(peer1.sendNoWait(PeerActor.Message.CloseConnectionForActor)))
        ) // simulate real actor finalizer
      (peer1.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = false, applicationLevel = false))
        .returns(Applicative[F].unit)
      (peer1.sendNoWait _)
        .expects(PeerActor.Message.GetNetworkQuality)
        .returns(Applicative[F].unit)
      (peer1.sendNoWait _).expects(PeerActor.Message.CloseConnectionForActor).returns(Applicative[F].unit)

      val initialPeersMap: Map[HostId, Peer[F]] =
        Map(buildSimplePeerEntry(PeerState.Cold, None, host1Id, host1Ra, remoteNetworkLevel = false))

      val mockData = buildDefaultMockData(networkAlgebra = networkAlgebra, initialPeers = initialPeersMap)
      buildActorFromMockData(mockData)
        .use { actor =>
          for {
            initialState <- actor.send(PeersManager.Message.SetupRequestsProxy(requestProxy))
            _ = assert(initialState.peersHandler(host1Id).state == PeerState.Cold)
            withUpdate <- actor.send(
              buildOpenedPeerConnectionMessage(client1, ConnectedPeer(host1Ra, host1Id.id, NetworkProtocolVersions.V1))
            )
            _ = assert(withUpdate.peersHandler(host1Id).state == PeerState.Cold)
            _ = assert(withUpdate.peersHandler(host1Id).asServer.get.address == host1Ra)
            _ = assert(withUpdate.peersHandler(host1Id).asServer.get.peerId == host1Id)
            _ = assert(withUpdate.peersHandler(host1Id).actorOpt.get == peer1)
          } yield ()
        }
    }
  }

  test("Create new peer actor, if no as server then use previous value") {
    withMock {
      val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]
      val requestProxy = mock[RequestsProxyActor[F]]

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
            .onFinalize(Sync[F].delay(peer1.sendNoWait(PeerActor.Message.CloseConnectionForActor)))
        ) // simulate real actor finalizer
      val connectedPeer = ConnectedPeer(host1Ra, host1Id.id, NetworkProtocolVersions.V1)
      (() => client1.remotePeer).expects().anyNumberOfTimes().returns(connectedPeer)

      (peer1.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = false, applicationLevel = false))
        .returns(Applicative[F].unit)
      (peer1.sendNoWait _)
        .expects(PeerActor.Message.GetNetworkQuality)
        .returns(Applicative[F].unit)
      (peer1.sendNoWait _).expects(PeerActor.Message.CloseConnectionForActor).returns(Applicative[F].unit)

      val initialPeersMap: Map[HostId, Peer[F]] =
        Map(buildSimplePeerEntry(PeerState.Cold, None, host1Id, host1Ra, remoteNetworkLevel = false))

      val mockData = buildDefaultMockData(networkAlgebra = networkAlgebra, initialPeers = initialPeersMap)
      buildActorFromMockData(mockData)
        .use { actor =>
          for {
            initialState <- actor.send(PeersManager.Message.SetupRequestsProxy(requestProxy))
            _ = assert(initialState.peersHandler(host1Id).state == PeerState.Cold)
            withUpdate <- actor.send(PeersManager.Message.OpenedPeerConnection(client1, None))
            _ = assert(withUpdate.peersHandler(host1Id).state == PeerState.Cold)
            _ = assert(withUpdate.peersHandler(host1Id).asServer.get.address.some == host1Ra.some)
            _ = assert(withUpdate.peersHandler(host1Id).actorOpt.get == peer1)
          } yield ()
        }
    }
  }

  test("Self connection shall be declined") {
    withMock {
      val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]
      val requestProxy = mock[RequestsProxyActor[F]]

      val client1 = mock[BlockchainPeerClient[F]]
      (() => client1.closeConnection()).expects().returning(Applicative[F].unit)
      val host1Id = arbitraryHost.arbitrary.first
      val host1Ra = RemoteAddress("first", 1)

      val initialPeersMap: Map[HostId, Peer[F]] =
        Map(buildSimplePeerEntry(PeerState.Cold, None, host1Id, host1Ra, remoteNetworkLevel = false))

      val mockData = buildDefaultMockData(networkAlgebra = networkAlgebra, initialPeers = initialPeersMap)
      buildActorFromMockData(mockData)
        .use { actor =>
          for {
            _ <- actor.send(PeersManager.Message.SetupRequestsProxy(requestProxy))
            withUpdate <- actor.send(
              buildOpenedPeerConnectionMessage(
                client1,
                ConnectedPeer(RemoteAddress("second", 1), thisHostId.id, NetworkProtocolVersions.V1)
              )
            )
            _ = assert(withUpdate.peersHandler(thisHostId).state == PeerState.Banned)
          } yield ()
        }
    }
  }

  test("Reputation update: move warm to hot") {
    withMock {
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

      val mockData =
        buildDefaultMockData(p2pConfig = p2pConfig, initialPeers = initialPeersMap, hotPeersUpdate = hotUpdater)
      buildActorFromMockData(mockData)
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
      val blockChecker: BlockCheckerActor[F] = mock[BlockCheckerActor[F]]
      val requestProxy: RequestsProxyActor[F] = mock[RequestsProxyActor[F]]

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
      (peer5.sendNoWait _)
        .expects(PeerActor.Message.GetNetworkQuality)
        .returns(Applicative[F].unit)

      val host6Id = arbitraryHost.arbitrary.first
      val host6Ra = RemoteAddress("6", 6)
      val peer6 = mockPeerActor[F]()
      val client6 = mock[BlockchainPeerClient[F]]
      (() => client6.closeConnection()).stubs().returning(Applicative[F].unit)

      val host7Id = arbitraryHost.arbitrary.first
      val host7Ra = RemoteAddress("7", 7)
      val peer7 = mockPeerActor[F]()
      val client7 = mock[BlockchainPeerClient[F]]
      (() => client7.closeConnection()).stubs().returning(Applicative[F].unit)

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

      val mockData = buildDefaultMockData(initialPeers = initialPeersMap, networkAlgebra = networkAlgebra)
      buildActorFromMockData(mockData)
        .use { actor =>
          for {
            _ <- actor.send(PeersManager.Message.SetupBlockChecker(blockChecker))
            _ <- actor.send(PeersManager.Message.SetupRequestsProxy(requestProxy))
            stateHost1 <- actor.send(
              buildOpenedPeerConnectionMessage(client1, ConnectedPeer(host1Ra, host1Id.id, NetworkProtocolVersions.V1))
            )
            _ = assert(stateHost1.peersHandler(host1Id).state == PeerState.Cold)
            _ = assert(stateHost1.peersHandler(host1Id).closedTimestamps == Seq(1))
            _ = assert(stateHost1.peersHandler(host1Id).asServer.get.address == host1Ra)
            _ = assert(stateHost1.peersHandler(host1Id).asServer.get.peerId == host1Id)
            stateHost2 <- actor.send(
              buildOpenedPeerConnectionMessage(client2, ConnectedPeer(host2Ra, host2Id.id, NetworkProtocolVersions.V1))
            )
            _ = assert(stateHost2.peersHandler(host2Id).state == PeerState.Cold)
            _ = assert(stateHost2.peersHandler(host2Id).closedTimestamps == Seq.empty)
            _ = assert(stateHost2.peersHandler(host2Id).asServer.get.address == host2Ra)
            _ = assert(stateHost2.peersHandler(host2Id).asServer.get.peerId == host2Id)
            stateHost3 <- actor.send(
              buildOpenedPeerConnectionMessage(client3, ConnectedPeer(host3Ra, host3Id.id, NetworkProtocolVersions.V1))
            )
            _ = assert(stateHost3.peersHandler(host3Id).state == PeerState.Warm)
            _ = assert(stateHost3.peersHandler(host3Id).closedTimestamps == Seq(3))
            _ = assert(stateHost3.peersHandler(host3Id).asServer.get.address == host3Ra)
            _ = assert(stateHost3.peersHandler(host3Id).asServer.get.peerId == host3Id)
            stateHost4 <- actor.send(
              buildOpenedPeerConnectionMessage(client4, ConnectedPeer(host4Ra, host4Id.id, NetworkProtocolVersions.V1))
            )
            _ = assert(stateHost4.peersHandler(host4Id).state == PeerState.Banned)
            _ = assert(stateHost4.peersHandler(host4Id).closedTimestamps == Seq(4))
            _ = assert(stateHost4.peersHandler(host4Id).asServer.isEmpty)
            stateHost5 <- actor.send(
              buildOpenedPeerConnectionMessage(
                client5,
                ConnectedPeer(host5Ra, host5Id.id, NetworkProtocolVersions.V1),
                false
              )
            )
            _ = assert(stateHost5.peersHandler(host5Id).state == PeerState.Hot)
            _ = assert(stateHost5.peersHandler(host5Id).closedTimestamps == Seq(5))
            _ = assert(stateHost5.peersHandler(host5Id).asServer == client5RemotePort)
            stateHost6 <- actor.send(
              buildOpenedPeerConnectionMessage(client6, ConnectedPeer(host6Ra, host6Id.id, NetworkProtocolVersions.V1))
            )
            _ = assert(stateHost6.peersHandler(host6Id).state == PeerState.Cold)
            _ = assert(stateHost6.peersHandler(host6Id).closedTimestamps == Seq(6))
            _ = assert(stateHost6.peersHandler(host6Id).asServer.isEmpty)
            stateHost7 <- actor.send(
              buildOpenedPeerConnectionMessage(client7, ConnectedPeer(host7Ra, host7Id.id, NetworkProtocolVersions.V1))
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
      val blockChecker = mock[BlockCheckerActor[F]]
      val requestProxy = mock[RequestsProxyActor[F]]

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

      val mockData = buildDefaultMockData(networkAlgebra = networkAlgebra, initialPeers = initialPeersMap)
      buildActorFromMockData(mockData)
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
      val blockChecker = mock[BlockCheckerActor[F]]
      val requestProxy = mock[RequestsProxyActor[F]]

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
        KnownRemotePeer(host2Id, RemoteAddress(host2.reverse, hostServer2Port), 0, 0, None),
        KnownRemotePeer(host3Id, RemoteAddress(host3.reverse, hostServer3Port), 0, 0, None),
        KnownRemotePeer(host4Id, RemoteAddress(host4.reverse, hostServer4Port), 0, 0, None),
        KnownRemotePeer(host5Id, RemoteAddress(host5.reverse, hostServer5Port), 0, 0, None)
      )
      (writingHosts.apply _).expects(expectedHosts).once().returns(().pure[F])

      implicit val dummyReverseReverseDns: ReverseDnsResolver[F] = (h: String) => h.reverse.pure[F]
      val mockData = buildDefaultMockData(initialPeers = initialPeersMap, savePeersFunction = writingHosts)
      buildActorFromMockData(mockData, dummyDns, dummyReverseReverseDns)
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
      val blockChecker = mock[BlockCheckerActor[F]]
      val requestProxy = mock[RequestsProxyActor[F]]

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
        remoteAddresses.map(rp => KnownRemotePeer(rp.peerId, rp.address, 0, 0, None))

      val mockData = buildDefaultMockData()
      buildActorFromMockData(mockData)
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
      val blockChecker = mock[BlockCheckerActor[F]]
      val requestProxy = mock[RequestsProxyActor[F]]

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

      val mockData = buildDefaultMockData(initialPeers = initialPeersMap)
      buildActorFromMockData(mockData)
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
      val blockChecker = mock[BlockCheckerActor[F]]
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

      val initialCash = defaultCache()
      initialCash.putAll(blockWithSource.asJava)

      val initialNewRep = 1L
      val peer: PeerActor[F] = mockPeerActor[F]()
      val initialPeersMap: Map[HostId, Peer[F]] = Map(
        buildSimplePeerEntry(PeerState.Hot, Option(peer), blockSource, newRep = initialNewRep),
        buildSimplePeerEntry(PeerState.Cold, Option(mockPeerActor[F]()), coldPeer),
        buildSimplePeerEntry(PeerState.Warm, Option(mockPeerActor[F]()), preWarmPeer),
        buildSimplePeerEntry(PeerState.Banned, Option(mockPeerActor[F]()), bannedPeer),
        buildSimplePeerEntry(PeerState.Warm, Option(mockPeerActor[F]()), warmPeer)
      )

      val expectedMessage = PeerActor.Message.DownloadBlockHeaders(NonEmptyChain(blockHeader1.id, blockHeader2.id))
      (peer.sendNoWait _).expects(expectedMessage).once().returns(Applicative[F].unit)

      val messageToSend =
        PeersManager.Message.BlockHeadersRequest(None, NonEmptyChain(blockHeader1.id, blockHeader2.id))

      val mockData = buildDefaultMockData(initialPeers = initialPeersMap, blockSource = initialCash)
      buildActorFromMockData(mockData)
        .use { actor =>
          for {
            _        <- actor.send(PeersManager.Message.SetupBlockChecker(blockChecker))
            _        <- actor.send(PeersManager.Message.SetupRequestsProxy(requestProxy))
            endState <- actor.send(messageToSend)
            _ = assert(endState.peersHandler.peers(blockSource).newRep == initialNewRep + 1)
          } yield ()
        }
    }

  }

  test("Request for block header download shall be sent to one of the peers if only hint is available") {
    withMock {
      val blockChecker = mock[BlockCheckerActor[F]]
      val requestProxy = mock[RequestsProxyActor[F]]

      val blockHeader1 = ModelGenerators.arbitraryHeader.arbitrary.first.embedId
      val blockHeader2 = ModelGenerators.arbitraryHeader.arbitrary.first.embedId

      val blockSource: HostId = arbitraryHost.arbitrary.first

      val blockWithSource: Map[BlockId, Set[HostId]] = Map()

      val initialCash = defaultCache()
      initialCash.putAll(blockWithSource.asJava)

      val peer: PeerActor[F] = mockPeerActor[F]()
      val initialPeersMap: Map[HostId, Peer[F]] =
        Map(buildSimplePeerEntry(PeerState.Hot, Option(peer), blockSource, newRep = Long.MaxValue - 1))

      val expectedMessage = PeerActor.Message.DownloadBlockHeaders(NonEmptyChain(blockHeader1.id, blockHeader2.id))
      (peer.sendNoWait _).expects(expectedMessage).once().returns(Applicative[F].unit)

      val messageToSend =
        PeersManager.Message.BlockHeadersRequest(Option(blockSource), NonEmptyChain(blockHeader1.id, blockHeader2.id))

      val mockData = buildDefaultMockData(initialPeers = initialPeersMap, blockSource = initialCash)
      buildActorFromMockData(mockData)
        .use { actor =>
          for {
            _        <- actor.send(PeersManager.Message.SetupBlockChecker(blockChecker))
            _        <- actor.send(PeersManager.Message.SetupRequestsProxy(requestProxy))
            endState <- actor.send(messageToSend)
            _ = assert(endState.peersHandler.peers(blockSource).newRep == mockData.p2pConfig.maxPeerNovelty)
          } yield ()
        }

    }
  }

  test("Request for block header download shall not be sent to one of the peers because of bad peer state") {
    withMock {
      val blockChecker = mock[BlockCheckerActor[F]]
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

      val initialCash = defaultCache()
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

      val mockData = buildDefaultMockData(initialPeers = initialPeersMap, blockSource = initialCash)
      buildActorFromMockData(mockData)
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
      val blockChecker = mock[BlockCheckerActor[F]]
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

      val initialCash = defaultCache()
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

      val mockData = buildDefaultMockData(initialPeers = initialPeersMap, blockSource = initialCash)
      buildActorFromMockData(mockData)
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
      val blockChecker = mock[BlockCheckerActor[F]]
      val requestProxy = mock[RequestsProxyActor[F]]

      val blockHeader1 = ModelGenerators.arbitraryHeader.arbitrary.first.embedId
      val blockHeader2 = ModelGenerators.arbitraryHeader.arbitrary.first.embedId

      val blockSource: HostId = arbitraryHost.arbitrary.first

      val blockWithSource: Map[BlockId, Set[HostId]] = Map()

      val initialCash = defaultCache()
      initialCash.putAll(blockWithSource.asJava)

      val peer: PeerActor[F] = mockPeerActor[F]()
      val initialPeersMap: Map[HostId, Peer[F]] = Map(buildSimplePeerEntry(PeerState.Hot, Option(peer), blockSource))

      val expectedMessage = PeerActor.Message.DownloadBlockBodies(NonEmptyChain(blockHeader1, blockHeader2))
      (peer.sendNoWait _).expects(expectedMessage).once().returns(Applicative[F].unit)

      val messageToSend =
        PeersManager.Message.BlockBodyRequest(Option(blockSource), NonEmptyChain(blockHeader1, blockHeader2))

      val mockData = buildDefaultMockData(initialPeers = initialPeersMap, blockSource = initialCash)
      buildActorFromMockData(mockData)
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
      val blockChecker = mock[BlockCheckerActor[F]]
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

      val initialCash = defaultCache()
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

      val mockData = buildDefaultMockData(initialPeers = initialPeersMap, blockSource = initialCash)
      buildActorFromMockData(mockData)
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
      val blockChecker = mock[BlockCheckerActor[F]]
      val requestProxy = mock[RequestsProxyActor[F]]

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
      val mockData = buildDefaultMockData(initialPeers = initialPeersMap)
      buildActorFromMockData(mockData)
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
      val blockChecker = mock[BlockCheckerActor[F]]
      val requestProxy = mock[RequestsProxyActor[F]]

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

      val mockData = buildDefaultMockData(initialPeers = initialPeersMap)
      buildActorFromMockData(mockData)
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
      val blockChecker = mock[BlockCheckerActor[F]]
      val requestProxy = mock[RequestsProxyActor[F]]

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

      val mockData = buildDefaultMockData(initialPeers = initialPeersMap)
      buildActorFromMockData(mockData)
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
      val blockChecker = mock[BlockCheckerActor[F]]
      val requestProxy = mock[RequestsProxyActor[F]]

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

      val mockData = buildDefaultMockData(initialPeers = initialPeersMap)
      buildActorFromMockData(mockData)
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

  test("Non critical error message shall be processed") {
    withMock {
      val blockChecker = mock[BlockCheckerActor[F]]
      val requestProxy = mock[RequestsProxyActor[F]]

      val peerActor = mockPeerActor[F]()
      (peerActor.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = false, applicationLevel = false))
        .returning(Applicative[F].unit)

      val initialPeersMap: Map[HostId, Peer[F]] = Map(
        buildSimplePeerEntry(
          PeerState.Hot,
          Option(peerActor),
          hostId
        )
      )

      val mockData = buildDefaultMockData(initialPeers = initialPeersMap)
      buildActorFromMockData(mockData)
        .use { actor =>
          for {
            _        <- actor.send(PeersManager.Message.SetupBlockChecker(blockChecker))
            _        <- actor.send(PeersManager.Message.SetupRequestsProxy(requestProxy))
            newState <- actor.send(PeersManager.Message.NonCriticalErrorForHost(hostId))
            peer = newState.peersHandler(hostId)
            _ = assert(peer.state == PeerState.Cold)
          } yield ()
        }
    }
  }

  test("Performance reputation after header downloading shall be updated") {
    withMock {
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

      val mockData = buildDefaultMockData(initialPeers = initialPeersMap)
      buildActorFromMockData(mockData)
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

        val mockData = buildDefaultMockData(initialPeers = initialPeersMap)
        buildActorFromMockData(mockData)
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
      val host1 = arbitraryHost.arbitrary.first
      val host2 = arbitraryHost.arbitrary.first
      val host3 = arbitraryHost.arbitrary.first
      val host4 = arbitraryHost.arbitrary.first
      val host5 = arbitraryHost.arbitrary.first
      val host6 = arbitraryHost.arbitrary.first
      val host7 = arbitraryHost.arbitrary.first

      val blockId1 = arbitraryBlockId.arbitrary.first
      val blockId2 = arbitraryBlockId.arbitrary.first
      val blockId3 = arbitraryBlockId.arbitrary.first
      val blockId4 = arbitraryBlockId.arbitrary.first
      val blockId5 = arbitraryBlockId.arbitrary.first
      val blockId6 = arbitraryBlockId.arbitrary.first

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
        host5 -> blockId5,
        host3 -> blockId6,
        host6 -> blockId2,
        host7 -> blockId2
      )

      val initialPeersMap: Map[HostId, Peer[F]] = Map(
        buildSimplePeerEntry(PeerState.Hot, Option(mockPeerActor[F]()), host1, blockRep = worstReputation),
        buildSimplePeerEntry(PeerState.Hot, Option(mockPeerActor[F]()), host2, blockRep = worstReputation),
        buildSimplePeerEntry(PeerState.Hot, Option(mockPeerActor[F]()), host3, blockRep = worstReputation),
        buildSimplePeerEntry(PeerState.Hot, Option(mockPeerActor[F]()), host4, blockRep = worstReputation),
        buildSimplePeerEntry(PeerState.Hot, Option(mockPeerActor[F]()), host5, blockRep = worstedReputation),
        buildSimplePeerEntry(PeerState.Hot, Option(mockPeerActor[F]()), host6, blockRep = bestReputation),
        buildSimplePeerEntry(PeerState.Hot, Option(mockPeerActor[F]()), host7, blockRep = bestReputation)
      )

      val cache = Caffeine.newBuilder.maximumSize(blockSourceCacheSize).build[BlockId, Set[HostId]]()
      cache.put(blockId5, Set(host1, host2, host3))
      cache.put(blockId2, Set(host7))

      val mockData = buildDefaultMockData(initialPeers = initialPeersMap, blockSource = cache)
      buildActorFromMockData(mockData)
        .use { actor =>
          for {
            res <- actor.send(PeersManager.Message.BlocksSource(update))
            _ = assert(res.peersHandler(host1).blockRep == bestReputation)
            _ = assert(res.peersHandler(host1).newRep == mockData.p2pConfig.remotePeerNoveltyInSlots)
            _ = assert(res.peersHandler(host2).blockRep == okReputation)
            _ = assert(res.peersHandler(host3).blockRep == bestReputation)
            _ = assert(res.peersHandler(host4).blockRep == worstReputation)
            _ = assert(res.peersHandler(host5).blockRep == worstedReputation)
            _ = assert(res.peersHandler(host6).blockRep == bestReputation)
          } yield ()
        }
    }
  }

  test("Move remote peer to cold if remote host provide bad k lookback slot data") {
    withMock {
      val peerActor = mockPeerActor[F]()
      val initialPeersMap: Map[HostId, Peer[F]] = Map(buildSimplePeerEntry(PeerState.Hot, Option(peerActor), hostId))

      (peerActor.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = false, applicationLevel = false))
        .returns(Applicative[F].unit)

      val mockData = buildDefaultMockData(initialPeers = initialPeersMap)
      buildActorFromMockData(mockData)
        .use { actor =>
          for {
            newState <- actor.send(PeersManager.Message.BadKLookbackSlotData(hostId))
            _ = assert(newState.peersHandler(hostId).state == PeerState.Cold)
          } yield ()
        }
    }
  }

  test("Updated timestamps, Not useful cold peers shall be cleared as outdated timestamps") {
    withMock {
      val host1Id = arbitraryHost.arbitrary.first
      val host1 = "1"
      val hostServer1Port = 1
      val host1Ra = RemoteAddress(host1, hostServer1Port)
      val actor1 = mock[PeerActor[F]]

      val host2Id = arbitraryHost.arbitrary.first
      val host2 = "2"
      val hostServer2Port = 2
      val host2Ra = RemoteAddress(host2, hostServer2Port)
      val actor2 = mock[PeerActor[F]]

      val host3Id = arbitraryHost.arbitrary.first
      val host3 = "3"
      val hostServer3Port = 3
      val host3Ra = RemoteAddress(host3, hostServer3Port)
      val actor3 = mock[PeerActor[F]]

      val host4Id = arbitraryHost.arbitrary.first
      val host4 = "4"
      val hostServer4Port = 4
      val host4Ra = RemoteAddress(host4, hostServer4Port)
      val actor4 = mock[PeerActor[F]]

      val host5Id = arbitraryHost.arbitrary.first
      val host5 = "5"
      val hostServer5Port = 5
      val host5Ra = RemoteAddress(host5, hostServer5Port)

      val host6Id = arbitraryHost.arbitrary.first
      val host6 = "6"
      val hostServer6Port = 6
      val host6Ra = RemoteAddress(host6, hostServer6Port)

      val host7Id = arbitraryHost.arbitrary.first
      val host7 = "7"
      val hostServer7Port = 7
      val host7Ra = RemoteAddress(host7, hostServer7Port)

      val host8Id = arbitraryHost.arbitrary.first
      val host8 = "8"
      val hostServer8Port = 8
      val host8Ra = RemoteAddress(host8, hostServer8Port)
      val host8Timestamp = System.currentTimeMillis()

      val host9Id = arbitraryHost.arbitrary.first
      val host9 = "9"
      val hostServer9Port = 9
      val host9Ra = RemoteAddress(host9, hostServer9Port)

      val host10Id = arbitraryHost.arbitrary.first
      val host10 = "10"
      val hostServer10Port = 10
      val host10Ra = RemoteAddress(host10, hostServer10Port)
      val actor10 = mock[PeerActor[F]]

      val host11Id = arbitraryHost.arbitrary.first
      val host11 = "11"
      val hostServer11Port = 11
      val host11Ra = RemoteAddress(host11, hostServer11Port)
      val host11Timestamp = 0L

      val networkConfig =
        defaultP2PConfig.networkProperties.copy(
          maximumEligibleColdConnections = 10,
          minimumEligibleColdConnections = 10,
          closeTimeoutWindowInMs = 20000,
          closeTimeoutFirstDelayInMs = 1000000
        )
      val config = defaultP2PConfig.copy(networkProperties = networkConfig)

      val initialPeersMap: Map[HostId, Peer[F]] = Map(
        buildSimplePeerEntry(PeerState.Cold, actor1.some, host1Id, host1Ra, noServer = true, remoteNetworkLevel = true),
        buildSimplePeerEntry(
          PeerState.Cold,
          actor2.some,
          host2Id,
          host2Ra,
          noServer = true,
          remoteNetworkLevel = false
        ),
        buildSimplePeerEntry(
          PeerState.Cold,
          actor3.some,
          host3Id,
          host3Ra,
          noServer = false,
          remoteNetworkLevel = true
        ),
        buildSimplePeerEntry(
          PeerState.Cold,
          actor4.some,
          host4Id,
          host4Ra,
          noServer = false,
          remoteNetworkLevel = false
        ),
        buildSimplePeerEntry(
          PeerState.Cold,
          None,
          host5Id,
          host5Ra,
          noServer = true,
          remoteNetworkLevel = true,
          closedTimestamps = Seq(0)
        ),
        buildSimplePeerEntry(PeerState.Cold, None, host6Id, host6Ra, noServer = true, remoteNetworkLevel = false),
        buildSimplePeerEntry(
          PeerState.Cold,
          None,
          host7Id,
          host7Ra,
          noServer = false,
          remoteNetworkLevel = true,
          closedTimestamps = Seq(System.currentTimeMillis())
        ),
        buildSimplePeerEntry(
          PeerState.Cold,
          None,
          host8Id,
          host8Ra,
          noServer = false,
          remoteNetworkLevel = false,
          connectionTimestamp = host8Timestamp.some
        ),
        buildSimplePeerEntry(
          PeerState.Cold,
          None,
          host9Id,
          host9Ra,
          noServer = false,
          remoteNetworkLevel = false,
          connectionTimestamp = (System.currentTimeMillis() - networkConfig.clearColdIfNotActiveForInMs - 1).some
        ),
        buildSimplePeerEntry(
          PeerState.Cold,
          actor10.some,
          host10Id,
          host10Ra,
          noServer = false,
          remoteNetworkLevel = false,
          connectionTimestamp = 0L.some
        ),
        buildSimplePeerEntry(
          PeerState.Cold,
          None,
          host11Id,
          host11Ra,
          noServer = false,
          remoteNetworkLevel = true,
          connectionTimestamp = host11Timestamp.some
        )
      )

      val mockData = buildDefaultMockData(initialPeers = initialPeersMap, p2pConfig = config)
      buildActorFromMockData(mockData)
        .use { actor =>
          for {
            timeNow  <- Sync[F].realTime.map(_.toMillis)
            newState <- actor.send(PeersManager.Message.UpdatePeersTick)
            _ = assert(newState.peersHandler.getColdPeers.size == 9)
            _ = assert(newState.peersHandler.getColdPeers.contains(host1Id))
            _ = assert(newState.peersHandler.getColdPeers(host1Id).activeConnectionTimestamp.get > timeNow)
            _ = assert(newState.peersHandler.getColdPeers.contains(host2Id))
            _ = assert(newState.peersHandler.getColdPeers(host2Id).activeConnectionTimestamp.get > timeNow)
            _ = assert(newState.peersHandler.getColdPeers.contains(host3Id))
            _ = assert(newState.peersHandler.getColdPeers(host3Id).activeConnectionTimestamp.get > timeNow)
            _ = assert(newState.peersHandler.getColdPeers.contains(host4Id))
            _ = assert(newState.peersHandler.getColdPeers(host4Id).activeConnectionTimestamp.get > timeNow)
            _ = assert(newState.peersHandler.getColdPeers.contains(host5Id))
            _ = assert(newState.peersHandler.getColdPeers(host5Id).activeConnectionTimestamp.isEmpty)
            _ = assert(newState.peersHandler.getColdPeers(host5Id).closedTimestamps.isEmpty)
            _ = assert(newState.peersHandler.getColdPeers.contains(host7Id))
            _ = assert(newState.peersHandler.getColdPeers(host7Id).closedTimestamps.size == 1)
            _ = assert(newState.peersHandler.getColdPeers(host7Id).activeConnectionTimestamp.isEmpty)
            _ = assert(newState.peersHandler.getColdPeers.contains(host8Id))
            _ = assert(newState.peersHandler.getColdPeers(host8Id).activeConnectionTimestamp.get == host8Timestamp)
            _ = assert(newState.peersHandler.getColdPeers.contains(host10Id))
            _ = assert(newState.peersHandler.getColdPeers(host10Id).activeConnectionTimestamp.get > timeNow)
            _ = assert(newState.peersHandler.getColdPeers.contains(host11Id))
            _ = assert(newState.peersHandler.getColdPeers(host11Id).activeConnectionTimestamp.get == host11Timestamp)
          } yield ()
        }
    }
  }

  test("Not useful cold peers shall be cleared, save minimumColdConnections active peers first") {
    withMock {
      val host1Id = arbitraryHost.arbitrary.first
      val host1 = "1"
      val hostServer1Port = 1
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

      val host7Id = arbitraryHost.arbitrary.first
      val host7 = "7"
      val hostServer7Port = 7
      val host7Ra = RemoteAddress(host7, hostServer7Port)

      val host8Id = arbitraryHost.arbitrary.first
      val host8 = "8"
      val hostServer8Port = 8
      val host8Ra = RemoteAddress(host8, hostServer8Port)

      val curTime = System.currentTimeMillis()
      val initialPeersMap: Map[HostId, Peer[F]] = Map(
        buildSimplePeerEntry(
          PeerState.Cold,
          None,
          host1Id,
          host1Ra,
          remoteNetworkLevel = false,
          closedTimestamps = Seq(curTime)
        ),
        buildSimplePeerEntry(
          PeerState.Cold,
          None,
          host2Id,
          host2Ra,
          remoteNetworkLevel = false,
          closedTimestamps = Seq(curTime, curTime)
        ),
        buildSimplePeerEntry(
          PeerState.Cold,
          None,
          host3Id,
          host3Ra,
          remoteNetworkLevel = false,
          closedTimestamps = Seq(curTime, curTime, curTime)
        ),
        buildSimplePeerEntry(
          PeerState.Cold,
          None,
          host4Id,
          host4Ra,
          remoteNetworkLevel = false,
          closedTimestamps = Seq(curTime, curTime, curTime, curTime)
        ),
        buildSimplePeerEntry(
          PeerState.Cold,
          None,
          host5Id,
          host5Ra,
          remoteNetworkLevel = false,
          closedTimestamps = Seq(curTime, curTime, curTime, curTime, curTime)
        ),
        buildSimplePeerEntry(
          PeerState.Cold,
          None,
          host6Id,
          host6Ra,
          remoteNetworkLevel = false,
          closedTimestamps = Seq(curTime, curTime, curTime, curTime, curTime, curTime)
        ),
        buildSimplePeerEntry(
          PeerState.Cold,
          None,
          host7Id,
          host7Ra,
          remoteNetworkLevel = false,
          closedTimestamps = Seq(curTime, curTime, curTime, curTime, curTime, curTime, curTime)
        ),
        buildSimplePeerEntry(
          PeerState.Cold,
          None,
          host8Id,
          host8Ra,
          remoteNetworkLevel = false,
          closedTimestamps = Seq(curTime, curTime, curTime, curTime, curTime, curTime, curTime, curTime)
        )
      )

      val networkConfig =
        defaultP2PConfig.networkProperties.copy(
          maximumEligibleColdConnections = 7,
          minimumEligibleColdConnections = 3,
          closeTimeoutWindowInMs = 20000,
          closeTimeoutFirstDelayInMs = 1000000
        )

      val config = defaultP2PConfig.copy(networkProperties = networkConfig)
      val mockData = buildDefaultMockData(initialPeers = initialPeersMap, p2pConfig = config)
      buildActorFromMockData(mockData)
        .use { actor =>
          for {
            newState <- actor.send(PeersManager.Message.UpdatePeersTick)
            _ = assert(newState.peersHandler.getColdPeers.size == 3)
            _ = assert(newState.peersHandler.getColdPeers.contains(host1Id))
            _ = assert(newState.peersHandler.getColdPeers.contains(host2Id))
            _ = assert(newState.peersHandler.getColdPeers.contains(host3Id))
          } yield ()
        }
    }
  }

  test("Not useful cold peers shall be cleared, save active peers first") {
    withMock {
      val host1Id = arbitraryHost.arbitrary.first
      val host1 = "1"
      val hostServer1Port = 1
      val host1Ra = RemoteAddress(host1, hostServer1Port)
      val actor1 = mock[PeerActor[F]]

      val host2Id = arbitraryHost.arbitrary.first
      val host2 = "2"
      val hostServer2Port = 2
      val host2Ra = RemoteAddress(host2, hostServer2Port)
      val actor2 = mock[PeerActor[F]]

      val host3Id = arbitraryHost.arbitrary.first
      val host3 = "3"
      val hostServer3Port = 3
      val host3Ra = RemoteAddress(host3, hostServer3Port)
      val actor3 = mock[PeerActor[F]]

      val host4Id = arbitraryHost.arbitrary.first
      val host4 = "4"
      val hostServer4Port = 4
      val host4Ra = RemoteAddress(host4, hostServer4Port)
      val actor4 = mock[PeerActor[F]]

      val host5Id = arbitraryHost.arbitrary.first
      val host5 = "5"
      val hostServer5Port = 5
      val host5Ra = RemoteAddress(host5, hostServer5Port)

      val host6Id = arbitraryHost.arbitrary.first
      val host6 = "6"
      val hostServer6Port = 6
      val host6Ra = RemoteAddress(host6, hostServer6Port)

      val host7Id = arbitraryHost.arbitrary.first
      val host7 = "7"
      val hostServer7Port = 7
      val host7Ra = RemoteAddress(host7, hostServer7Port)

      val host8Id = arbitraryHost.arbitrary.first
      val host8 = "8"
      val hostServer8Port = 8
      val host8Ra = RemoteAddress(host8, hostServer8Port)

      val initialPeersMap: Map[HostId, Peer[F]] = Map(
        buildSimplePeerEntry(PeerState.Cold, actor1.some, host1Id, host1Ra, noServer = true, remoteNetworkLevel = true),
        buildSimplePeerEntry(
          PeerState.Cold,
          actor2.some,
          host2Id,
          host2Ra,
          noServer = true,
          remoteNetworkLevel = false
        ),
        buildSimplePeerEntry(
          PeerState.Cold,
          actor3.some,
          host3Id,
          host3Ra,
          noServer = false,
          remoteNetworkLevel = true
        ),
        buildSimplePeerEntry(
          PeerState.Cold,
          actor4.some,
          host4Id,
          host4Ra,
          noServer = false,
          remoteNetworkLevel = false
        ),
        buildSimplePeerEntry(PeerState.Cold, None, host5Id, host5Ra, noServer = true, remoteNetworkLevel = true),
        buildSimplePeerEntry(PeerState.Cold, None, host6Id, host6Ra, noServer = true, remoteNetworkLevel = false),
        buildSimplePeerEntry(PeerState.Cold, None, host7Id, host7Ra, noServer = false, remoteNetworkLevel = true),
        buildSimplePeerEntry(PeerState.Cold, None, host8Id, host8Ra, noServer = false, remoteNetworkLevel = false)
      )

      val networkConfig =
        defaultP2PConfig.networkProperties.copy(
          maximumEligibleColdConnections = 6,
          minimumEligibleColdConnections = 6,
          closeTimeoutWindowInMs = 20000,
          closeTimeoutFirstDelayInMs = 1000000
        )

      val config = defaultP2PConfig.copy(networkProperties = networkConfig)
      val mockData = buildDefaultMockData(initialPeers = initialPeersMap, p2pConfig = config)
      buildActorFromMockData(mockData)
        .use { actor =>
          for {
            newState <- actor.send(PeersManager.Message.UpdatePeersTick)
            _ = assert(newState.peersHandler.getColdPeers.size == 6)
            _ = assert(newState.peersHandler.getColdPeers.contains(host1Id))
            _ = assert(newState.peersHandler.getColdPeers.contains(host2Id))
            _ = assert(newState.peersHandler.getColdPeers.contains(host3Id))
            _ = assert(newState.peersHandler.getColdPeers.contains(host4Id))
            _ = assert(newState.peersHandler.getColdPeers.contains(host5Id))
            _ = assert(newState.peersHandler.getColdPeers.contains(host7Id))
          } yield ()
        }
    }
  }

  test("Not useful cold peers shall be cleared but always save active peers") {
    withMock {
      val host1Id = arbitraryHost.arbitrary.first
      val host1 = "1"
      val hostServer1Port = 1
      val host1Ra = RemoteAddress(host1, hostServer1Port)
      val actor1 = mock[PeerActor[F]]

      val host2Id = arbitraryHost.arbitrary.first
      val host2 = "2"
      val hostServer2Port = 2
      val host2Ra = RemoteAddress(host2, hostServer2Port)
      val actor2 = mock[PeerActor[F]]

      val host3Id = arbitraryHost.arbitrary.first
      val host3 = "3"
      val hostServer3Port = 3
      val host3Ra = RemoteAddress(host3, hostServer3Port)
      val actor3 = mock[PeerActor[F]]

      val host4Id = arbitraryHost.arbitrary.first
      val host4 = "4"
      val hostServer4Port = 4
      val host4Ra = RemoteAddress(host4, hostServer4Port)
      val actor4 = mock[PeerActor[F]]

      val host5Id = arbitraryHost.arbitrary.first
      val host5 = "5"
      val hostServer5Port = 5
      val host5Ra = RemoteAddress(host5, hostServer5Port)

      val host6Id = arbitraryHost.arbitrary.first
      val host6 = "6"
      val hostServer6Port = 6
      val host6Ra = RemoteAddress(host6, hostServer6Port)

      val host7Id = arbitraryHost.arbitrary.first
      val host7 = "7"
      val hostServer7Port = 7
      val host7Ra = RemoteAddress(host7, hostServer7Port)

      val host8Id = arbitraryHost.arbitrary.first
      val host8 = "8"
      val hostServer8Port = 8
      val host8Ra = RemoteAddress(host8, hostServer8Port)

      val initialPeersMap: Map[HostId, Peer[F]] = Map(
        buildSimplePeerEntry(PeerState.Cold, actor1.some, host1Id, host1Ra, noServer = true, remoteNetworkLevel = true),
        buildSimplePeerEntry(
          PeerState.Cold,
          actor2.some,
          host2Id,
          host2Ra,
          noServer = true,
          remoteNetworkLevel = false
        ),
        buildSimplePeerEntry(
          PeerState.Cold,
          actor3.some,
          host3Id,
          host3Ra,
          noServer = false,
          remoteNetworkLevel = true
        ),
        buildSimplePeerEntry(
          PeerState.Cold,
          actor4.some,
          host4Id,
          host4Ra,
          noServer = false,
          remoteNetworkLevel = false
        ),
        buildSimplePeerEntry(PeerState.Cold, None, host5Id, host5Ra, noServer = true, remoteNetworkLevel = true),
        buildSimplePeerEntry(PeerState.Cold, None, host6Id, host6Ra, noServer = true, remoteNetworkLevel = false),
        buildSimplePeerEntry(PeerState.Cold, None, host7Id, host7Ra, noServer = false, remoteNetworkLevel = true),
        buildSimplePeerEntry(PeerState.Cold, None, host8Id, host8Ra, noServer = false, remoteNetworkLevel = false)
      )

      val networkConfig =
        defaultP2PConfig.networkProperties.copy(
          maximumEligibleColdConnections = 1,
          minimumEligibleColdConnections = 1,
          closeTimeoutWindowInMs = 20000,
          closeTimeoutFirstDelayInMs = 1000000
        )

      val config = defaultP2PConfig.copy(networkProperties = networkConfig)
      val mockData = buildDefaultMockData(initialPeers = initialPeersMap, p2pConfig = config)
      buildActorFromMockData(mockData)
        .use { actor =>
          for {
            newState <- actor.send(PeersManager.Message.UpdatePeersTick)
            _ = assert(newState.peersHandler.getColdPeers.size == 6)
            _ = assert(newState.peersHandler.getColdPeers.contains(host1Id))
            _ = assert(newState.peersHandler.getColdPeers.contains(host2Id))
            _ = assert(newState.peersHandler.getColdPeers.contains(host3Id))
            _ = assert(newState.peersHandler.getColdPeers.contains(host4Id))
            _ = assert(newState.peersHandler.getColdPeers.contains(host5Id))
            _ = assert(newState.peersHandler.getColdPeers.contains(host7Id))
          } yield ()
        }
    }
  }

  test("Not useful cold peers shall be cleared, save active peers first and then peers with less close events") {
    withMock {
      val host1Id = arbitraryHost.arbitrary.first
      val host1 = "1"
      val hostServer1Port = 1
      val host1Ra = RemoteAddress(host1, hostServer1Port)
      val actor1 = mock[PeerActor[F]]

      val host2Id = arbitraryHost.arbitrary.first
      val host2 = "2"
      val hostServer2Port = 2
      val host2Ra = RemoteAddress(host2, hostServer2Port)
      val actor2 = mock[PeerActor[F]]

      val host3Id = arbitraryHost.arbitrary.first
      val host3 = "3"
      val hostServer3Port = 3
      val host3Ra = RemoteAddress(host3, hostServer3Port)
      val actor3 = mock[PeerActor[F]]

      val host4Id = arbitraryHost.arbitrary.first
      val host4 = "4"
      val hostServer4Port = 4
      val host4Ra = RemoteAddress(host4, hostServer4Port)
      val actor4 = mock[PeerActor[F]]

      val host5Id = arbitraryHost.arbitrary.first
      val host5 = "5"
      val hostServer5Port = 5
      val host5Ra = RemoteAddress(host5, hostServer5Port)

      val host6Id = arbitraryHost.arbitrary.first
      val host6 = "6"
      val hostServer6Port = 6
      val host6Ra = RemoteAddress(host6, hostServer6Port)

      val host7Id = arbitraryHost.arbitrary.first
      val host7 = "7"
      val hostServer7Port = 7
      val host7Ra = RemoteAddress(host7, hostServer7Port)

      val host8Id = arbitraryHost.arbitrary.first
      val host8 = "8"
      val hostServer8Port = 8
      val host8Ra = RemoteAddress(host8, hostServer8Port)

      val initialPeersMap: Map[HostId, Peer[F]] = Map(
        buildSimplePeerEntry(PeerState.Cold, actor1.some, host1Id, host1Ra, noServer = true, remoteNetworkLevel = true),
        buildSimplePeerEntry(
          PeerState.Cold,
          actor2.some,
          host2Id,
          host2Ra,
          noServer = true,
          remoteNetworkLevel = false
        ),
        buildSimplePeerEntry(
          PeerState.Cold,
          actor3.some,
          host3Id,
          host3Ra,
          noServer = false,
          remoteNetworkLevel = true
        ),
        buildSimplePeerEntry(
          PeerState.Cold,
          actor4.some,
          host4Id,
          host4Ra,
          noServer = false,
          remoteNetworkLevel = false
        ),
        buildSimplePeerEntry(PeerState.Cold, None, host5Id, host5Ra, noServer = true, remoteNetworkLevel = true),
        buildSimplePeerEntry(
          PeerState.Cold,
          None,
          host6Id,
          host6Ra,
          noServer = false,
          remoteNetworkLevel = false,
          closedTimestamps = Seq(System.currentTimeMillis())
        ),
        buildSimplePeerEntry(PeerState.Cold, None, host7Id, host7Ra, noServer = false, remoteNetworkLevel = true),
        buildSimplePeerEntry(PeerState.Cold, None, host8Id, host8Ra, noServer = false, remoteNetworkLevel = false)
      )

      val networkConfig =
        defaultP2PConfig.networkProperties.copy(
          maximumEligibleColdConnections = 7,
          minimumEligibleColdConnections = 7,
          closeTimeoutWindowInMs = 20000,
          closeTimeoutFirstDelayInMs = 1000000
        )

      val config = defaultP2PConfig.copy(networkProperties = networkConfig)
      val mockData = buildDefaultMockData(initialPeers = initialPeersMap, p2pConfig = config)
      buildActorFromMockData(mockData)
        .use { actor =>
          for {
            newState <- actor.send(PeersManager.Message.UpdatePeersTick)
            _ = assert(newState.peersHandler.getColdPeers.size == 7)
            _ = assert(newState.peersHandler.getColdPeers.contains(host1Id))
            _ = assert(newState.peersHandler.getColdPeers.contains(host2Id))
            _ = assert(newState.peersHandler.getColdPeers.contains(host3Id))
            _ = assert(newState.peersHandler.getColdPeers.contains(host4Id))
            _ = assert(newState.peersHandler.getColdPeers.contains(host5Id))
            _ = assert(newState.peersHandler.getColdPeers.contains(host8Id))
          } yield ()
        }
    }
  }

  test("Warm peers shall be requested") {
    withMock {
      val maximumWarmConnections = 3

      val host1Id = arbitraryHost.arbitrary.first
      val host1 = "1"
      val hostServer1Port = 2
      val host1Ra = RemoteAddress(host1, hostServer1Port)
      val actor1 = mock[PeerActor[F]]
      (actor1.sendNoWait _)
        .expects(PeerActor.Message.GetHotPeersFromPeer(maximumWarmConnections))
        .returning(Applicative[F].unit)

      val initialPeersMap: Map[HostId, Peer[F]] =
        Map(buildSimplePeerEntry(PeerState.Hot, actor1.some, host1Id, host1Ra))

      val networkConfig = defaultP2PConfig.networkProperties.copy(maximumWarmConnections = maximumWarmConnections)

      val config = defaultP2PConfig.copy(networkProperties = networkConfig)
      val mockData = buildDefaultMockData(initialPeers = initialPeersMap, p2pConfig = config)
      buildActorFromMockData(mockData)
        .use { actor =>
          for {
            newState <- actor.send(PeersManager.Message.UpdatePeersTick)
            _ = assert(newState.peersHandler.getHotPeers.size == 1)
          } yield ()
        }
    }
  }

  test("Warm peers shall not be requested if enough warm peers are present") {
    withMock {
      val maximumWarmConnections = 1

      val host1Id = arbitraryHost.arbitrary.first
      val host1 = "1"
      val hostServer1Port = 2
      val host1Ra = RemoteAddress(host1, hostServer1Port)
      val actor1 = mock[PeerActor[F]]
      (actor1.sendNoWait _)
        .expects(PeerActor.Message.GetHotPeersFromPeer(maximumWarmConnections))
        .never()
        .returning(Applicative[F].unit)

      val host2Id = arbitraryHost.arbitrary.first
      val host2 = "2"
      val hostServer2Port = 2
      val host2Ra = RemoteAddress(host2, hostServer2Port)
      val actor2 = mock[PeerActor[F]]

      val initialPeersMap: Map[HostId, Peer[F]] = Map(
        buildSimplePeerEntry(PeerState.Hot, actor1.some, host1Id, host1Ra),
        buildSimplePeerEntry(PeerState.Warm, actor2.some, host2Id, host2Ra)
      )

      val networkConfig = defaultP2PConfig.networkProperties.copy(maximumWarmConnections = maximumWarmConnections)

      val config = defaultP2PConfig.copy(networkProperties = networkConfig)
      val mockData = buildDefaultMockData(initialPeers = initialPeersMap, p2pConfig = config)
      buildActorFromMockData(mockData)
        .use { actor =>
          for {
            newState <- actor.send(PeersManager.Message.UpdatePeersTick)
            _ = assert(newState.peersHandler.getHotPeers.size == 1)
          } yield ()
        }
    }
  }

  test("Updated remote node id shall be processed: normal id change") {
    withMock {
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
        0,
        None
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
          0,
          None
        )
      )

      val mockData = buildDefaultMockData(initialPeers = initialPeersMap)
      buildActorFromMockData(mockData)
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

  test("Updated remote node id shall be processed: non exist id change") {
    withMock {
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
        0,
        None
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
          0,
          None
        )
      )

      val mockData = buildDefaultMockData(initialPeers = initialPeersMap)
      buildActorFromMockData(mockData)
        .use { actor =>
          for {
            newState <- actor.send(PeersManager.Message.RemotePeerIdChanged(host1IdNew, host1IdOld))
            _ = assert(newState.peersHandler.peers.size == initialPeersMap.size + 1) // +1 because of self-banned peer
            _ = assert(newState.peersHandler.getColdPeers.contains(host2Id))
            _ = assert(newState.peersHandler.peers.contains(host1IdOld))
            _ = assert(newState.peersHandler.peers(host1IdOld) == host1Peer)
          } yield ()
        }
    }
  }

  test("Updated remote node id shall be processed: close peer if try to update the peer id to already exist peer") {
    withMock {
      val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]

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
            .onFinalize(Sync[F].delay(peer1.sendNoWait(PeerActor.Message.CloseConnectionForActor)))
        ) // simulate real actor finalizer

      (peer1.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = false, applicationLevel = false))
        .returning(Applicative[F].unit)
      (peer1.sendNoWait _).expects(PeerActor.Message.GetNetworkQuality).returning(Applicative[F].unit)
      (peer1.sendNoWait _).expects(PeerActor.Message.CloseConnectionForActor).returning(Applicative[F].unit)

      val initialPeersMap: Map[HostId, Peer[F]] = Map(
        buildSimplePeerEntry(PeerState.Hot, host2PeerActor.some, host2Id)
      )

      val mockData = buildDefaultMockData(initialPeers = initialPeersMap, networkAlgebra = networkAlgebra)
      buildActorFromMockData(mockData)
        .use { actor =>
          for {
            _ <- actor.send(PeersManager.Message.SetupRequestsProxy(mock[RequestsProxyActor[F]]))
            newState1 <-
              actor.send(
                buildOpenedPeerConnectionMessage(
                  client1,
                  ConnectedPeer(host1Ra, host1Id.id, NetworkProtocolVersions.V1)
                )
              )
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

  test("Updated remote node id shall be processed: close peer if try to update the peer id to already banned peer") {
    withMock {
      val networkAlgebra: NetworkAlgebra[F] = mock[NetworkAlgebra[F]]

      val host1Id = arbitraryHost.arbitrary.first
      val host1Ra = RemoteAddress("1", 1)
      val peer1 = mockPeerActor[F]()
      val client1 = mock[BlockchainPeerClient[F]]

      val host2Id = arbitraryHost.arbitrary.first

      (networkAlgebra.makePeer _)
        .expects(host1Id, *, client1, *, *, *, *, *, *, *, *, *, *, *, *)
        .once()
        .returns(
          Resource
            .pure(peer1)
            .onFinalize(Sync[F].delay(peer1.sendNoWait(PeerActor.Message.CloseConnectionForActor)))
        ) // simulate real actor finalizer

      (peer1.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = false, applicationLevel = false))
        .returning(Applicative[F].unit)
      (peer1.sendNoWait _).expects(PeerActor.Message.GetNetworkQuality).returning(Applicative[F].unit)
      (peer1.sendNoWait _).expects(PeerActor.Message.CloseConnectionForActor).returning(Applicative[F].unit)

      val initialPeersMap: Map[HostId, Peer[F]] = Map(buildSimplePeerEntry(PeerState.Banned, None, host2Id))

      val mockData = buildDefaultMockData(initialPeers = initialPeersMap, networkAlgebra = networkAlgebra)
      buildActorFromMockData(mockData)
        .use { actor =>
          for {
            _ <- actor.send(PeersManager.Message.SetupRequestsProxy(mock[RequestsProxyActor[F]]))
            newState1 <-
              actor.send(
                buildOpenedPeerConnectionMessage(
                  client1,
                  ConnectedPeer(host1Ra, host1Id.id, NetworkProtocolVersions.V1)
                )
              )
            _ = assert(newState1.peersHandler.peers(host1Id).state == PeerState.Cold)
            newState2 <- actor.send(PeersManager.Message.RemotePeerIdChanged(host1Id, host2Id))
            _ = assert(!newState2.peersHandler.peers.contains(host1Id))
            _ = assert(newState2.peersHandler.peers(host2Id).state == PeerState.Banned)
          } yield ()
        }
    }
  }

  test("PeerManager shall self-ban own peer id to avoid self-connections") {
    withMock {
      val mockData = buildDefaultMockData()
      buildActorFromMockData(mockData)
        .use { actor =>
          for {
            newState <- actor.send(PeersManager.Message.SetupBlockChecker(mock[BlockCheckerActor[F]]))
            _ = assert(newState.peersHandler.peers(thisHostId).state == PeerState.Banned)
          } yield ()
        }
    }
  }

  test("PeerManager shall add local address on request") {
    withMock {
      val localAddress = arbitraryRemoteAddress.arbitrary.first
      val mockData = buildDefaultMockData()
      buildActorFromMockData(mockData)
        .use { actor =>
          for {
            newState <- actor.send(PeersManager.Message.UpdateThisPeerAddress(localAddress))
            _ = assert(newState.thisHostIps.head == localAddress.host)
          } yield ()
        }
    }
  }
}
