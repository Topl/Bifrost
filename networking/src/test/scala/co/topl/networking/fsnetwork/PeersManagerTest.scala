package co.topl.networking.fsnetwork

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

  val hostId: HostId = RemoteAddress("127.0.0.1", 0)

  val coldToWarmSelector: ColdToWarmSelector = (coldHosts: Set[HostId], countToReceive: Int) =>
    coldHosts.toSeq.sortBy(_.port).take(countToReceive).toSet

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
        Map.empty[HostId, Peer[F]] + (hostId -> Peer(PeerState.Hot, Option(peerActor)))

      PeersManager
        .makeActor(
          networkAlgebra,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          headerToBodyValidation,
          newPeerCreationAlgebra,
          p2pConfig,
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
        Map.empty[HostId, Peer[F]] + (hostId -> Peer(PeerState.Hot, Option(peerActor)))

      PeersManager
        .makeActor(
          networkAlgebra,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          headerToBodyValidation,
          newPeerCreationAlgebra,
          p2pConfig,
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
          networkAlgebra,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          headerToBodyValidation,
          newPeerCreationAlgebra,
          p2pConfig,
          coldToWarmSelector = coldToWarmSelector
        )
        .use { actor =>
          for {
            _            <- actor.send(PeersManager.Message.SetupReputationAggregator(reputationAggregator))
            withColdPeer <- actor.send(PeersManager.Message.AddKnownPeers(NonEmptyChain(host1, host2, host3)))
            _ = assert(withColdPeer.peers(host1).state == PeerState.Cold)
            _ = assert(withColdPeer.peers(host2).state == PeerState.Cold)
            _ = assert(withColdPeer.peers(host3).state == PeerState.Cold)

            withUpdate <- actor.send(PeersManager.Message.UpdatedReputation(Map.empty, Map.empty, Map.empty))
            _ = assert(withUpdate.peers(host1).state == PeerState.PreWarm)
            _ = assert(withUpdate.peers(host2).state == PeerState.PreWarm)
            _ = assert(withUpdate.peers(host3).state == PeerState.Cold)
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
        .expects(ReputationAggregator.Message.PeerIsCold(host5))
        .returns(().pure[F])

      val host6 = RemoteAddress("six", 6)
      val peer6 = mock[PeerActor[F]]
      (peer6.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(networkLevel = false, applicationLevel = false))
        .returns(().pure[F])
      (() => peer6.id).expects().anyNumberOfTimes().returns(6) // used in release actor function
      (reputationAggregator.sendNoWait _)
        .expects(ReputationAggregator.Message.PeerIsCold(host6))
        .returns(().pure[F])

      val initialPeersMap: Map[HostId, Peer[F]] =
        Map(
          host1 -> Peer(PeerState.Hot, Option(peer1)),
          host2 -> Peer(PeerState.Hot, Option(peer2)),
          host3 -> Peer(PeerState.Hot, Option(peer3)),
          host4 -> Peer(PeerState.Hot, Option(peer4)),
          host5 -> Peer(PeerState.Hot, Option(peer5)),
          host6 -> Peer(PeerState.Hot, Option(peer6))
        )

      val performanceRep: Map[HostId, HostReputationValue] =
        Map(
          host1 -> 1.0,
          host3 -> p2pConfig.networkProperties.minimumRequiredReputation * 1.05
        )

      val blockRep: Map[HostId, HostReputationValue] =
        Map(
          host2 -> 1.0,
          host3 -> p2pConfig.networkProperties.minimumRequiredReputation * 1.05
        )

      val noveltyRep: Map[HostId, Long] =
        Map(
          host4 -> 1
        )

      PeersManager
        .makeActor(
          networkAlgebra,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          headerToBodyValidation,
          newPeerCreationAlgebra,
          p2pConfig,
          coldToWarmSelector = coldToWarmSelector,
          initialPeers = initialPeersMap
        )
        .use { actor =>
          for {
            _          <- actor.send(PeersManager.Message.SetupReputationAggregator(reputationAggregator))
            withUpdate <- actor.send(PeersManager.Message.UpdatedReputation(performanceRep, blockRep, noveltyRep))
            _ = assert(withUpdate.peers(host1).state == PeerState.Hot)
            _ = assert(withUpdate.peers(host2).state == PeerState.Hot)
            _ = assert(withUpdate.peers(host3).state == PeerState.Hot)
            _ = assert(withUpdate.peers(host4).state == PeerState.Hot)
            _ = assert(withUpdate.peers(host5).state == PeerState.Cold)
            _ = assert(withUpdate.peers(host6).state == PeerState.Cold)
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
          host1 -> Peer(PeerState.Warm, Option(peer1)),
          host2 -> Peer(PeerState.Warm, Option(peer2)),
          host3 -> Peer(PeerState.Warm, Option(peer3)),
          host4 -> Peer(PeerState.Warm, Option(peer4))
        )

      (reputationAggregator.sendNoWait _)
        .expects(ReputationAggregator.Message.NewHotPeer(NonEmptyChain(host2, host1)))
        .once()
        .returns(().pure[F])

      val performanceRep: Map[HostId, HostReputationValue] =
        Map(
          host1 -> 1.0,
          host2 -> 0.9,
          host3 -> 0.8,
          host4 -> 0.7
        )

      PeersManager
        .makeActor(
          networkAlgebra,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          headerToBodyValidation,
          newPeerCreationAlgebra,
          p2pConfig,
          coldToWarmSelector = coldToWarmSelector,
          initialPeers = initialPeersMap
        )
        .use { actor =>
          for {
            _          <- actor.send(PeersManager.Message.SetupReputationAggregator(reputationAggregator))
            withUpdate <- actor.send(PeersManager.Message.UpdatedReputation(performanceRep, Map.empty, Map.empty))
            _ = assert(withUpdate.peers(host1).state == PeerState.Hot)
            _ = assert(withUpdate.peers(host2).state == PeerState.Hot)
            _ = assert(withUpdate.peers(host3).state == PeerState.Warm)
            _ = assert(withUpdate.peers(host4).state == PeerState.Warm)
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
      (networkAlgebra.makePeer _).expects(host1, *, *, *, *, *, *, *, *).once().returns(Resource.pure(peer1))
      (() => peer1.id).expects().anyNumberOfTimes().returns(1)
      (peer1.sendNoWait _)
        .expects(PeerActor.Message.GetNetworkQuality)
        .returns(().pure[F])
      (peer1.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(true, false))
        .returns(().pure[F])

      val host2 = RemoteAddress("second", 2)
      val peer2 = mock[PeerActor[F]]
      (networkAlgebra.makePeer _).expects(host2, *, *, *, *, *, *, *, *).once().returns(Resource.pure(peer2))
      (() => peer2.id).expects().anyNumberOfTimes().returns(2)
      (peer2.sendNoWait _)
        .expects(PeerActor.Message.GetNetworkQuality)
        .returns(().pure[F])
      (peer2.sendNoWait _)
        .expects(PeerActor.Message.UpdateState(true, false))
        .returns(().pure[F])

      val initialPeersMap = Map(host1 -> Peer(PeerState.PreWarm, None))

      PeersManager
        .makeActor(
          networkAlgebra,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          headerToBodyValidation,
          newPeerCreationAlgebra,
          p2pConfig,
          initialPeers = initialPeersMap
        )
        .use { actor =>
          for {
            _          <- actor.send(PeersManager.Message.SetupReputationAggregator(reputationAggregator))
            _          <- actor.send(PeersManager.Message.SetupBlockChecker(blockChecker))
            _          <- actor.send(PeersManager.Message.SetupRequestsProxy(requestProxy))
            stateHost1 <- actor.send(PeersManager.Message.OpenedPeerConnection(host1, mock[BlockchainPeerClient[F]]))
            _ = assert(stateHost1.peers(host1).state == PeerState.Warm)
            stateHost2 <- actor.send(PeersManager.Message.OpenedPeerConnection(host2, mock[BlockchainPeerClient[F]]))
            _ = assert(stateHost2.peers(host2).state == PeerState.Warm)
          } yield ()
        }
    }
  }

}
