package co.topl.networking.fsnetwork

import cats.Applicative
import cats.data.NonEmptyChain
import cats.effect.kernel.Async
import cats.effect.{IO, Resource}
import cats.implicits._
import co.topl.algebras.Store
import co.topl.brambl.generators.TransactionGenerator
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import co.topl.consensus.algebras.{BlockHeaderToBodyValidationAlgebra, LocalChainAlgebra}
import co.topl.consensus.models.{BlockId, SlotData}
import co.topl.eventtree.ParentChildTree
import co.topl.models.ModelGenerators.GenHelper
import co.topl.models.generators.consensus.ModelGenerators.arbitraryHeader
import co.topl.networking.KnownHostOps
import co.topl.networking.blockchain.BlockchainPeerClient
import co.topl.networking.fsnetwork.NetworkQualityError.{IncorrectPongMessage, NoPongMessage}
import co.topl.networking.fsnetwork.PeerActorTest.F
import co.topl.networking.fsnetwork.PeerBlockBodyFetcher.PeerBlockBodyFetcherActor
import co.topl.networking.fsnetwork.PeerBlockHeaderFetcher.PeerBlockHeaderFetcherActor
import co.topl.networking.fsnetwork.PeersManager.PeersManagerActor
import co.topl.networking.fsnetwork.ReputationAggregator.Message.PingPongMessagePing
import co.topl.networking.fsnetwork.ReputationAggregator.ReputationAggregatorActor
import co.topl.networking.fsnetwork.RequestsProxy.RequestsProxyActor
import co.topl.node.models.{CurrentKnownHostsReq, CurrentKnownHostsRes, KnownHost, PingMessage, PongMessage}
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import co.topl.models.generators.consensus.ModelGenerators._

import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}

object PeerActorTest {
  type F[A] = IO[A]
}

class PeerActorTest extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory with TransactionGenerator {
  implicit val logger: Logger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)

  val hostId: HostId = "127.0.0.1"

  test("Setting application level to true shall send start fetching stream message") {
    withMock {
      val genesis = arbitrarySlotData.arbitrary.first
      val peersManager = mock[PeersManagerActor[F]]
      val requestsProxy = mock[RequestsProxyActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      (() => localChain.genesis).expects().once().returning(genesis.pure[F])
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val transactionStore = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree = mock[ParentChildTree[F, BlockId]]
      val headerToBodyValidation = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val client = mock[BlockchainPeerClient[F]]
      (client
        .getRemoteBlockIdAtHeight(_: Long, _: Option[BlockId]))
        .expects(1L, genesis.slotId.blockId.some)
        .once()
        .returning(genesis.slotId.blockId.some.pure[F])
      val reputationAggregation = mock[ReputationAggregatorActor[F]]
      val networkAlgebra = mock[NetworkAlgebra[F]]
      val blockHeaderFetcher = mock[PeerBlockHeaderFetcherActor[F]]
      (() => blockHeaderFetcher.id).expects().anyNumberOfTimes().returns(1)
      (networkAlgebra.makePeerHeaderFetcher _).expects(*, *, *, *, *, *).returns(Resource.pure(blockHeaderFetcher))

      val blockBodyFetcher = mock[PeerBlockBodyFetcherActor[F]]
      (() => blockBodyFetcher.id).expects().anyNumberOfTimes().returns(2)
      (networkAlgebra.makePeerBodyFetcher _).expects(*, *, *, *, *).returns(Resource.pure(blockBodyFetcher))

      (blockHeaderFetcher.sendNoWait _).expects(PeerBlockHeaderFetcher.Message.StartActor).returns(Applicative[F].unit)
      (blockHeaderFetcher.sendNoWait _).expects(PeerBlockHeaderFetcher.Message.StopActor).returns(Applicative[F].unit)

      (blockBodyFetcher.sendNoWait _).expects(PeerBlockBodyFetcher.Message.StartActor).returns(Applicative[F].unit)
      (blockBodyFetcher.sendNoWait _).expects(PeerBlockBodyFetcher.Message.StopActor).returns(Applicative[F].unit)

      PeerActor
        .makeActor(
          hostId,
          networkAlgebra,
          client,
          requestsProxy,
          reputationAggregation,
          peersManager,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          headerToBodyValidation
        )
        .use { actor =>
          for {
            _ <- actor.send(PeerActor.Message.UpdateState(networkLevel = true, applicationLevel = true))
          } yield ()
        }
    }
  }

  test("Block header download shall be forwarded to header fetcher") {
    withMock {
      val genesis = arbitrarySlotData.arbitrary.first
      val peersManager = mock[PeersManagerActor[F]]
      val requestsProxy = mock[RequestsProxyActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      (() => localChain.genesis).expects().once().returning(genesis.pure[F])
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val transactionStore = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree = mock[ParentChildTree[F, BlockId]]
      val headerToBodyValidation = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val client = mock[BlockchainPeerClient[F]]
      (client
        .getRemoteBlockIdAtHeight(_: Long, _: Option[BlockId]))
        .expects(1L, genesis.slotId.blockId.some)
        .once()
        .returning(genesis.slotId.blockId.some.pure[F])
      val reputationAggregation = mock[ReputationAggregatorActor[F]]
      val networkAlgebra = mock[NetworkAlgebra[F]]
      val blockHeaderFetcher = mock[PeerBlockHeaderFetcherActor[F]]
      (() => blockHeaderFetcher.id).expects().anyNumberOfTimes().returns(1)
      (networkAlgebra.makePeerHeaderFetcher _).expects(*, *, *, *, *, *).returns(Resource.pure(blockHeaderFetcher))

      val blockBodyFetcher = mock[PeerBlockBodyFetcherActor[F]]
      (() => blockBodyFetcher.id).expects().anyNumberOfTimes().returns(2)
      (networkAlgebra.makePeerBodyFetcher _).expects(*, *, *, *, *).returns(Resource.pure(blockBodyFetcher))

      (blockHeaderFetcher.sendNoWait _).expects(PeerBlockHeaderFetcher.Message.StartActor).returns(Applicative[F].unit)
      (blockHeaderFetcher.sendNoWait _).expects(PeerBlockHeaderFetcher.Message.StopActor).returns(Applicative[F].unit)

      (blockBodyFetcher.sendNoWait _).expects(PeerBlockBodyFetcher.Message.StartActor).returns(Applicative[F].unit)
      (blockBodyFetcher.sendNoWait _).expects(PeerBlockBodyFetcher.Message.StopActor).returns(Applicative[F].unit)

      val blockHeader = arbitraryHeader.arbitrary.first.embedId
      (blockHeaderFetcher.sendNoWait _)
        .expects(PeerBlockHeaderFetcher.Message.DownloadBlockHeaders(NonEmptyChain.one(blockHeader.id)))
        .returns(Applicative[F].unit)

      PeerActor
        .makeActor(
          hostId,
          networkAlgebra,
          client,
          requestsProxy,
          reputationAggregation,
          peersManager,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          headerToBodyValidation
        )
        .use { actor =>
          for {
            _ <- actor.send(PeerActor.Message.UpdateState(networkLevel = true, applicationLevel = true))
            _ <- actor.send(PeerActor.Message.DownloadBlockHeaders(NonEmptyChain.one(blockHeader.id)))
          } yield ()
        }
    }
  }

  test("Block body download shall be forwarded to body fetcher") {
    withMock {
      val genesis = arbitrarySlotData.arbitrary.first
      val peersManager = mock[PeersManagerActor[F]]
      val requestsProxy = mock[RequestsProxyActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      (() => localChain.genesis).expects().once().returning(genesis.pure[F])
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val transactionStore = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree = mock[ParentChildTree[F, BlockId]]
      val headerToBodyValidation = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val client = mock[BlockchainPeerClient[F]]
      (client
        .getRemoteBlockIdAtHeight(_: Long, _: Option[BlockId]))
        .expects(1L, genesis.slotId.blockId.some)
        .once()
        .returning(genesis.slotId.blockId.some.pure[F])
      val reputationAggregation = mock[ReputationAggregatorActor[F]]
      val networkAlgebra = mock[NetworkAlgebra[F]]
      val blockHeaderFetcher = mock[PeerBlockHeaderFetcherActor[F]]
      (() => blockHeaderFetcher.id).expects().anyNumberOfTimes().returns(1)
      (networkAlgebra.makePeerHeaderFetcher _).expects(*, *, *, *, *, *).returns(Resource.pure(blockHeaderFetcher))

      val blockBodyFetcher = mock[PeerBlockBodyFetcherActor[F]]
      (() => blockBodyFetcher.id).expects().anyNumberOfTimes().returns(2)
      (networkAlgebra.makePeerBodyFetcher _).expects(*, *, *, *, *).returns(Resource.pure(blockBodyFetcher))

      (blockHeaderFetcher.sendNoWait _).expects(PeerBlockHeaderFetcher.Message.StartActor).returns(Applicative[F].unit)
      (blockHeaderFetcher.sendNoWait _).expects(PeerBlockHeaderFetcher.Message.StopActor).returns(Applicative[F].unit)

      (blockBodyFetcher.sendNoWait _).expects(PeerBlockBodyFetcher.Message.StartActor).returns(Applicative[F].unit)
      (blockBodyFetcher.sendNoWait _).expects(PeerBlockBodyFetcher.Message.StopActor).returns(Applicative[F].unit)

      val blockHeader = arbitraryHeader.arbitrary.first.embedId
      (blockBodyFetcher.sendNoWait _)
        .expects(PeerBlockBodyFetcher.Message.DownloadBlocks(NonEmptyChain.one(blockHeader)))
        .returns(Applicative[F].unit)

      PeerActor
        .makeActor(
          hostId,
          networkAlgebra,
          client,
          requestsProxy,
          reputationAggregation,
          peersManager,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          headerToBodyValidation
        )
        .use { actor =>
          for {
            _ <- actor.send(PeerActor.Message.UpdateState(networkLevel = true, applicationLevel = true))
            _ <- actor.send(PeerActor.Message.DownloadBlockBodies(NonEmptyChain.one(blockHeader)))
          } yield ()
        }
    }
  }

  test("Ping shall be started and result is sent to reputation aggregator") {
    withMock {
      val genesis = arbitrarySlotData.arbitrary.first
      val peersManager = mock[PeersManagerActor[F]]
      val requestsProxy = mock[RequestsProxyActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      (() => localChain.genesis).expects().once().returning(genesis.pure[F])
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val transactionStore = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree = mock[ParentChildTree[F, BlockId]]
      val headerToBodyValidation = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val networkAlgebra = mock[NetworkAlgebra[F]]
      val blockHeaderFetcher = mock[PeerBlockHeaderFetcherActor[F]]
      (() => blockHeaderFetcher.id).expects().anyNumberOfTimes().returns(1)
      (networkAlgebra.makePeerHeaderFetcher _).expects(*, *, *, *, *, *).returns(Resource.pure(blockHeaderFetcher))

      val blockBodyFetcher = mock[PeerBlockBodyFetcherActor[F]]
      (() => blockBodyFetcher.id).expects().anyNumberOfTimes().returns(2)
      (networkAlgebra.makePeerBodyFetcher _).expects(*, *, *, *, *).returns(Resource.pure(blockBodyFetcher))

      (blockHeaderFetcher.sendNoWait _).expects(PeerBlockHeaderFetcher.Message.StopActor).returns(Applicative[F].unit)

      (blockBodyFetcher.sendNoWait _).expects(PeerBlockBodyFetcher.Message.StopActor).returns(Applicative[F].unit)

      val pingDelay = FiniteDuration(10, MILLISECONDS)

      val client = mock[BlockchainPeerClient[F]]
      (client.getPongMessage _).expects(*).atLeastOnce().onCall { ping: PingMessage =>
        Async[F].delayBy(Option(PongMessage(ping.ping.reverse)).pure[F], pingDelay)
      }

      (client
        .getRemoteBlockIdAtHeight(_: Long, _: Option[BlockId]))
        .expects(1L, genesis.slotId.blockId.some)
        .once()
        .returning(genesis.slotId.blockId.some.pure[F])

      val reputationAggregation = mock[ReputationAggregatorActor[F]]
      (reputationAggregation.sendNoWait _).expects(*).atLeastOnce().onCall { message: ReputationAggregator.Message =>
        message match {
          case PingPongMessagePing(`hostId`, Right(t)) =>
            assert(t >= pingDelay.toMillis)
            ().pure[F]
          case _ => throw new IllegalStateException()
        }
      }

      PeerActor
        .makeActor(
          hostId,
          networkAlgebra,
          client,
          requestsProxy,
          reputationAggregation,
          peersManager,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          headerToBodyValidation
        )
        .use { actor =>
          for {
            _ <- actor.send(PeerActor.Message.UpdateState(networkLevel = true, applicationLevel = false))
            _ <- Async[F].andWait(actor.send(PeerActor.Message.GetNetworkQuality), pingDelay * 5)
          } yield ()
        }
    }

  }

  test("Ping shall be started: one success and two errors") {
    withMock {
      val genesis = arbitrarySlotData.arbitrary.first
      val peersManager = mock[PeersManagerActor[F]]
      val requestsProxy = mock[RequestsProxyActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      (() => localChain.genesis).expects().once().returning(genesis.pure[F])
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val transactionStore = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree = mock[ParentChildTree[F, BlockId]]
      val headerToBodyValidation = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val networkAlgebra = mock[NetworkAlgebra[F]]
      val blockHeaderFetcher = mock[PeerBlockHeaderFetcherActor[F]]
      (() => blockHeaderFetcher.id).expects().anyNumberOfTimes().returns(1)
      (networkAlgebra.makePeerHeaderFetcher _).expects(*, *, *, *, *, *).returns(Resource.pure(blockHeaderFetcher))

      val blockBodyFetcher = mock[PeerBlockBodyFetcherActor[F]]
      (() => blockBodyFetcher.id).expects().anyNumberOfTimes().returns(2)
      (networkAlgebra.makePeerBodyFetcher _).expects(*, *, *, *, *).returns(Resource.pure(blockBodyFetcher))

      (blockHeaderFetcher.sendNoWait _).expects(PeerBlockHeaderFetcher.Message.StopActor).returns(Applicative[F].unit)

      (blockBodyFetcher.sendNoWait _).expects(PeerBlockBodyFetcher.Message.StopActor).returns(Applicative[F].unit)

      val pingDelay: FiniteDuration = FiniteDuration(10, MILLISECONDS)

      val client = mock[BlockchainPeerClient[F]]
      (client.getPongMessage _).expects(*).once().onCall { ping: PingMessage =>
        Async[F].delayBy(Option(PongMessage(ping.ping.reverse)).pure[F], pingDelay)
      }
      (client.getPongMessage _).expects(*).once().onCall { _: PingMessage =>
        Option.empty[PongMessage].pure[F]
      }
      (client.getPongMessage _).expects(*).atLeastOnce().onCall { ping: PingMessage =>
        Option(PongMessage(ping.ping)).pure[F]
      }
      (client
        .getRemoteBlockIdAtHeight(_: Long, _: Option[BlockId]))
        .expects(1L, genesis.slotId.blockId.some)
        .once()
        .returning(genesis.slotId.blockId.some.pure[F])

      val reputationAggregation = mock[ReputationAggregatorActor[F]]
      (reputationAggregation.sendNoWait _).expects(*).once().onCall { message: ReputationAggregator.Message =>
        message match {
          case PingPongMessagePing(`hostId`, Right(t)) =>
            assert(t >= pingDelay.toMillis)
            ().pure[F]
          case _ => throw new IllegalStateException()
        }
      }
      (reputationAggregation.sendNoWait _).expects(*).once().onCall { message: ReputationAggregator.Message =>
        message match {
          case PingPongMessagePing(`hostId`, Left(NoPongMessage)) => ().pure[F]
          case _                                                  => throw new IllegalStateException()
        }
      }
      (reputationAggregation.sendNoWait _).expects(*).atLeastOnce().onCall { message: ReputationAggregator.Message =>
        message match {
          case PingPongMessagePing(`hostId`, Left(IncorrectPongMessage)) => ().pure[F]
          case _                                                         => throw new IllegalStateException()
        }
      }

      PeerActor
        .makeActor(
          hostId,
          networkAlgebra,
          client,
          requestsProxy,
          reputationAggregation,
          peersManager,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          headerToBodyValidation
        )
        .use { actor =>
          actor.send(PeerActor.Message.GetNetworkQuality) >>
          actor.send(PeerActor.Message.GetNetworkQuality) >>
          actor.send(PeerActor.Message.GetNetworkQuality)
        }
    }

  }

  test("Request to get current tip shall be forwarded to block header fetcher") {
    withMock {
      val genesis = arbitrarySlotData.arbitrary.first
      val peersManager = mock[PeersManagerActor[F]]
      val requestsProxy = mock[RequestsProxyActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      (() => localChain.genesis).expects().once().returning(genesis.pure[F])
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val transactionStore = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree = mock[ParentChildTree[F, BlockId]]
      val headerToBodyValidation = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val client = mock[BlockchainPeerClient[F]]
      (client
        .getRemoteBlockIdAtHeight(_: Long, _: Option[BlockId]))
        .expects(1L, genesis.slotId.blockId.some)
        .once()
        .returning(genesis.slotId.blockId.some.pure[F])
      val reputationAggregation = mock[ReputationAggregatorActor[F]]
      val networkAlgebra = mock[NetworkAlgebra[F]]
      val blockHeaderFetcher = mock[PeerBlockHeaderFetcherActor[F]]
      (() => blockHeaderFetcher.id).expects().anyNumberOfTimes().returns(1)
      (networkAlgebra.makePeerHeaderFetcher _).expects(*, *, *, *, *, *).returns(Resource.pure(blockHeaderFetcher))

      val blockBodyFetcher = mock[PeerBlockBodyFetcherActor[F]]
      (() => blockBodyFetcher.id).expects().anyNumberOfTimes().returns(2)
      (networkAlgebra.makePeerBodyFetcher _).expects(*, *, *, *, *).returns(Resource.pure(blockBodyFetcher))

      (blockHeaderFetcher.sendNoWait _).expects(PeerBlockHeaderFetcher.Message.StopActor).returns(Applicative[F].unit)
      (blockBodyFetcher.sendNoWait _).expects(PeerBlockBodyFetcher.Message.StopActor).returns(Applicative[F].unit)

      (blockHeaderFetcher.sendNoWait _)
        .expects(PeerBlockHeaderFetcher.Message.GetCurrentTip)
        .returns(Applicative[F].unit)

      PeerActor
        .makeActor(
          hostId,
          networkAlgebra,
          client,
          requestsProxy,
          reputationAggregation,
          peersManager,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          headerToBodyValidation
        )
        .use { actor =>
          for {
            _ <- actor.send(PeerActor.Message.GetCurrentTip)
          } yield ()
        }
    }
  }

  test("Request to get peer neighbours shall be processed if none empty known hosts received") {
    withMock {
      val genesis = arbitrarySlotData.arbitrary.first
      val peersManager = mock[PeersManagerActor[F]]
      val requestsProxy = mock[RequestsProxyActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      (() => localChain.genesis).expects().once().returning(genesis.pure[F])
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val transactionStore = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree = mock[ParentChildTree[F, BlockId]]
      val headerToBodyValidation = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val client = mock[BlockchainPeerClient[F]]
      (client
        .getRemoteBlockIdAtHeight(_: Long, _: Option[BlockId]))
        .expects(1L, genesis.slotId.blockId.some)
        .once()
        .returning(genesis.slotId.blockId.some.pure[F])
      val reputationAggregation = mock[ReputationAggregatorActor[F]]
      val networkAlgebra = mock[NetworkAlgebra[F]]
      val blockHeaderFetcher = mock[PeerBlockHeaderFetcherActor[F]]
      (() => blockHeaderFetcher.id).expects().anyNumberOfTimes().returns(1)
      (networkAlgebra.makePeerHeaderFetcher _).expects(*, *, *, *, *, *).returns(Resource.pure(blockHeaderFetcher))

      val blockBodyFetcher = mock[PeerBlockBodyFetcherActor[F]]
      (() => blockBodyFetcher.id).expects().anyNumberOfTimes().returns(2)
      (networkAlgebra.makePeerBodyFetcher _).expects(*, *, *, *, *).returns(Resource.pure(blockBodyFetcher))

      (blockHeaderFetcher.sendNoWait _).expects(PeerBlockHeaderFetcher.Message.StopActor).returns(Applicative[F].unit)
      (blockBodyFetcher.sendNoWait _).expects(PeerBlockBodyFetcher.Message.StopActor).returns(Applicative[F].unit)

      val host1 = KnownHost("0.0.0.1", 1)
      val host2 = KnownHost("0.0.0.2", 2)
      val hosts = Seq(host1, host2)
      (client.getRemoteKnownHosts _)
        .expects(CurrentKnownHostsReq(2))
        .returns(Option(CurrentKnownHostsRes(hosts)).pure[F])

      (peersManager.sendNoWait _)
        .expects(
          PeersManager.Message.AddKnownPeers(NonEmptyChain.fromSeq(hosts.map(_.asRemoteAddress)).get)
        )
        .returns(Applicative[F].unit)

      PeerActor
        .makeActor(
          hostId,
          networkAlgebra,
          client,
          requestsProxy,
          reputationAggregation,
          peersManager,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          headerToBodyValidation
        )
        .use { actor =>
          for {
            _ <- actor.send(PeerActor.Message.GetHotPeersFromPeer(2))
          } yield ()
        }
    }
  }

  test("Request to get peer neighbours shall not be processed if empty known hosts received") {
    withMock {
      val genesis = arbitrarySlotData.arbitrary.first
      val peersManager = mock[PeersManagerActor[F]]
      val requestsProxy = mock[RequestsProxyActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      (() => localChain.genesis).expects().once().returning(genesis.pure[F])
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val transactionStore = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree = mock[ParentChildTree[F, BlockId]]
      val headerToBodyValidation = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val client = mock[BlockchainPeerClient[F]]
      (client
        .getRemoteBlockIdAtHeight(_: Long, _: Option[BlockId]))
        .expects(1L, genesis.slotId.blockId.some)
        .once()
        .returning(genesis.slotId.blockId.some.pure[F])
      val reputationAggregation = mock[ReputationAggregatorActor[F]]
      val networkAlgebra = mock[NetworkAlgebra[F]]
      val blockHeaderFetcher = mock[PeerBlockHeaderFetcherActor[F]]
      (() => blockHeaderFetcher.id).expects().anyNumberOfTimes().returns(1)
      (networkAlgebra.makePeerHeaderFetcher _).expects(*, *, *, *, *, *).returns(Resource.pure(blockHeaderFetcher))

      val blockBodyFetcher = mock[PeerBlockBodyFetcherActor[F]]
      (() => blockBodyFetcher.id).expects().anyNumberOfTimes().returns(2)
      (networkAlgebra.makePeerBodyFetcher _).expects(*, *, *, *, *).returns(Resource.pure(blockBodyFetcher))

      (blockHeaderFetcher.sendNoWait _).expects(PeerBlockHeaderFetcher.Message.StopActor).returns(Applicative[F].unit)
      (blockBodyFetcher.sendNoWait _).expects(PeerBlockBodyFetcher.Message.StopActor).returns(Applicative[F].unit)

      (client.getRemoteKnownHosts _)
        .expects(CurrentKnownHostsReq(2))
        .returns(Option(CurrentKnownHostsRes()).pure[F])

      PeerActor
        .makeActor(
          hostId,
          networkAlgebra,
          client,
          requestsProxy,
          reputationAggregation,
          peersManager,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          headerToBodyValidation
        )
        .use { actor =>
          for {
            _ <- actor.send(PeerActor.Message.GetHotPeersFromPeer(2))
          } yield ()
        }
    }
  }

  test("Request to get server port shall be processed") {
    withMock {
      val genesis = arbitrarySlotData.arbitrary.first
      val peersManager = mock[PeersManagerActor[F]]
      val requestsProxy = mock[RequestsProxyActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      (() => localChain.genesis).expects().once().returning(genesis.pure[F])
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val transactionStore = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree = mock[ParentChildTree[F, BlockId]]
      val headerToBodyValidation = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val client = mock[BlockchainPeerClient[F]]
      (client
        .getRemoteBlockIdAtHeight(_: Long, _: Option[BlockId]))
        .expects(1L, genesis.slotId.blockId.some)
        .once()
        .returning(genesis.slotId.blockId.some.pure[F])
      val reputationAggregation = mock[ReputationAggregatorActor[F]]
      val networkAlgebra = mock[NetworkAlgebra[F]]
      val blockHeaderFetcher = mock[PeerBlockHeaderFetcherActor[F]]
      (() => blockHeaderFetcher.id).expects().anyNumberOfTimes().returns(1)
      (networkAlgebra.makePeerHeaderFetcher _).expects(*, *, *, *, *, *).returns(Resource.pure(blockHeaderFetcher))

      val blockBodyFetcher = mock[PeerBlockBodyFetcherActor[F]]
      (() => blockBodyFetcher.id).expects().anyNumberOfTimes().returns(2)
      (networkAlgebra.makePeerBodyFetcher _).expects(*, *, *, *, *).returns(Resource.pure(blockBodyFetcher))

      (blockHeaderFetcher.sendNoWait _).expects(PeerBlockHeaderFetcher.Message.StopActor).returns(Applicative[F].unit)
      (blockBodyFetcher.sendNoWait _).expects(PeerBlockBodyFetcher.Message.StopActor).returns(Applicative[F].unit)

      val serverPort = 9085
      (() => client.remotePeerServerPort)
        .expects()
        .returns(Option(serverPort).pure[F])

      (peersManager.sendNoWait _)
        .expects(
          PeersManager.Message.RemotePeerServerPort(hostId, serverPort)
        )
        .returns(Applicative[F].unit)

      PeerActor
        .makeActor(
          hostId,
          networkAlgebra,
          client,
          requestsProxy,
          reputationAggregation,
          peersManager,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          headerToBodyValidation
        )
        .use { actor =>
          for {
            _ <- actor.send(PeerActor.Message.GetPeerServerAddress)
          } yield ()
        }
    }
  }
}
