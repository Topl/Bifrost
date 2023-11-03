package co.topl.networking.fsnetwork

import cats.{Applicative, MonadThrow, Show}
import cats.data.NonEmptyChain
import cats.effect.kernel.{Async, Sync}
import cats.effect.{IO, Resource}
import cats.implicits._
import co.topl.algebras.Store
import co.topl.brambl.generators.TransactionGenerator
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.validation.algebras.TransactionSyntaxVerifier
import co.topl.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import co.topl.consensus.algebras.{BlockHeaderToBodyValidationAlgebra, LocalChainAlgebra}
import co.topl.consensus.models.{BlockId, SlotData}
import co.topl.eventtree.{EventSourcedState, ParentChildTree}
import co.topl.ledger.algebras.MempoolAlgebra
import co.topl.models.ModelGenerators.GenHelper
import co.topl.models.generators.consensus.ModelGenerators._
import co.topl.networking.KnownHostOps
import co.topl.networking.blockchain.BlockchainPeerClient
import co.topl.networking.fsnetwork.NetworkQualityError.{IncorrectPongMessage, NoPongMessage}
import co.topl.networking.fsnetwork.PeerActorTest.F
import co.topl.networking.fsnetwork.PeerBlockBodyFetcher.PeerBlockBodyFetcherActor
import co.topl.networking.fsnetwork.PeerBlockHeaderFetcher.PeerBlockHeaderFetcherActor
import co.topl.networking.fsnetwork.PeerMempoolTransactionSync.PeerMempoolTransactionSyncActor
import co.topl.networking.fsnetwork.PeersManager.Message.PingPongMessagePing
import co.topl.networking.fsnetwork.PeersManager.PeersManagerActor
import co.topl.networking.fsnetwork.RequestsProxy.RequestsProxyActor
import co.topl.node.models._
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}

object PeerActorTest {
  type F[A] = IO[A]
}

class PeerActorTest extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory with TransactionGenerator {
  implicit val logger: Logger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)

  val hostId: HostId = "127.0.0.1"

  private val genesis = arbitrarySlotData.arbitrary.first

  private def createDummyClient(): BlockchainPeerClient[F] = {
    val client = mock[BlockchainPeerClient[F]]
    (client
      .getRemoteBlockIdAtHeight(_: Long, _: Option[BlockId]))
      .expects(1L, genesis.slotId.blockId.some)
      .once()
      .returning(genesis.slotId.blockId.some.pure[F])
    (client.getPongMessage _).stubs(*).onCall { ping: PingMessage =>
      Option(PongMessage(ping.ping.reverse)).pure[F]
    }
    (client.notifyAboutThisNetworkLevel _).stubs(*).returns(Applicative[F].unit)
    (client.closeConnection _).stubs().returns(Applicative[F].unit)

    client
  }

  private def createDummyNetworkAlgebra(): (
    NetworkAlgebra[F],
    PeerBlockHeaderFetcherActor[F],
    PeerBlockBodyFetcherActor[F],
    PeerMempoolTransactionSyncActor[F]
  ) = {
    val networkAlgebra = mock[NetworkAlgebra[F]]
    val blockHeaderFetcher = mock[PeerBlockHeaderFetcherActor[F]]
    (() => blockHeaderFetcher.id).expects().anyNumberOfTimes().returns(1)
    (networkAlgebra.makePeerHeaderFetcher _)
      .expects(*, *, *, *, *, *, *)
      .returns(
        // simulate real header fetcher behaviour on finalizing
        Resource
          .pure(blockHeaderFetcher)
          .onFinalize(Sync[F].defer(blockHeaderFetcher.sendNoWait(PeerBlockHeaderFetcher.Message.StopActor)))
      )

    val blockBodyFetcher = mock[PeerBlockBodyFetcherActor[F]]
    (() => blockBodyFetcher.id).expects().anyNumberOfTimes().returns(2)
    (networkAlgebra.makePeerBodyFetcher _)
      .expects(*, *, *, *, *, *)
      .returns(
        // simulate real body fetcher behaviour on finalizing
        Resource
          .pure(blockBodyFetcher)
          .onFinalize(Sync[F].defer(blockBodyFetcher.sendNoWait(PeerBlockBodyFetcher.Message.StopActor)))
      )

    val mempoolTransactionSync = mock[PeerMempoolTransactionSyncActor[F]]
    (() => mempoolTransactionSync.id).expects().anyNumberOfTimes().returns(3)
    (networkAlgebra.makeMempoolSyncFetcher _)
      .expects(*, *, *, *, *, *)
      .returns(
        // simulate real body fetcher behaviour on finalizing
        Resource
          .pure(mempoolTransactionSync)
          .onFinalize(Sync[F].defer(mempoolTransactionSync.sendNoWait(PeerMempoolTransactionSync.Message.StopActor)))
      )

    (blockHeaderFetcher.sendNoWait _).expects(PeerBlockHeaderFetcher.Message.StartActor).returns(Applicative[F].unit)
    (blockHeaderFetcher.sendNoWait _).expects(PeerBlockHeaderFetcher.Message.StopActor).returns(Applicative[F].unit)

    (blockBodyFetcher.sendNoWait _).expects(PeerBlockBodyFetcher.Message.StartActor).returns(Applicative[F].unit)
    (blockBodyFetcher.sendNoWait _).expects(PeerBlockBodyFetcher.Message.StopActor).returns(Applicative[F].unit)

    (mempoolTransactionSync.sendNoWait _)
      .expects(PeerMempoolTransactionSync.Message.StartActor)
      .returns(Applicative[F].unit)
    (mempoolTransactionSync.sendNoWait _)
      .expects(PeerMempoolTransactionSync.Message.StopActor)
      .returns(Applicative[F].unit)

    (networkAlgebra, blockHeaderFetcher, blockBodyFetcher, mempoolTransactionSync)
  }

  test("Setting application level to true shall send start fetching stream message") {
    withMock {
      val peersManager = mock[PeersManagerActor[F]]
      val requestsProxy = mock[RequestsProxyActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      (() => localChain.genesis).expects().once().returning(genesis.pure[F])
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val transactionStore = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree = mock[ParentChildTree[F, BlockId]]
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val transactionSyntaxValidation = mock[TransactionSyntaxVerifier[F]]
      val mempool = mock[MempoolAlgebra[F]]

      val client = createDummyClient()

      val (networkAlgebra, _, _, _) = createDummyNetworkAlgebra()
      (peersManager.sendNoWait _).stubs(*).returns(Applicative[F].unit)

      PeerActor
        .makeActor(
          hostId,
          networkAlgebra,
          client,
          requestsProxy,
          peersManager,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          blockHeights,
          headerToBodyValidation,
          transactionSyntaxValidation,
          mempool
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
      val peersManager = mock[PeersManagerActor[F]]
      val requestsProxy = mock[RequestsProxyActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      (() => localChain.genesis).expects().once().returning(genesis.pure[F])
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val transactionStore = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree = mock[ParentChildTree[F, BlockId]]
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val client = createDummyClient()
      val transactionSyntaxValidation = mock[TransactionSyntaxVerifier[F]]
      val mempool = mock[MempoolAlgebra[F]]
      (peersManager.sendNoWait _).stubs(*).returns(Applicative[F].unit)
      val (networkAlgebra, blockHeaderFetcher, _, _) = createDummyNetworkAlgebra()

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
          peersManager,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          blockHeights,
          headerToBodyValidation,
          transactionSyntaxValidation,
          mempool
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
      val peersManager = mock[PeersManagerActor[F]]
      val requestsProxy = mock[RequestsProxyActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      (() => localChain.genesis).expects().once().returning(genesis.pure[F])
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val transactionStore = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree = mock[ParentChildTree[F, BlockId]]
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val client = createDummyClient()
      val transactionSyntaxValidation = mock[TransactionSyntaxVerifier[F]]
      val mempool = mock[MempoolAlgebra[F]]
      (peersManager.sendNoWait _).stubs(*).returns(Applicative[F].unit)
      val (networkAlgebra, _, blockBodyFetcher, _) = createDummyNetworkAlgebra()

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
          peersManager,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          blockHeights,
          headerToBodyValidation,
          transactionSyntaxValidation,
          mempool
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
      val peersManager = mock[PeersManagerActor[F]]
      val requestsProxy = mock[RequestsProxyActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      (() => localChain.genesis).expects().once().returning(genesis.pure[F])
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val transactionStore = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree = mock[ParentChildTree[F, BlockId]]
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val networkAlgebra = mock[NetworkAlgebra[F]]
      val blockHeaderFetcher = mock[PeerBlockHeaderFetcherActor[F]]
      val transactionSyntaxValidation = mock[TransactionSyntaxVerifier[F]]
      val mempool = mock[MempoolAlgebra[F]]

      (() => blockHeaderFetcher.id).expects().anyNumberOfTimes().returns(1)
      (networkAlgebra.makePeerHeaderFetcher _).expects(*, *, *, *, *, *, *).returns(Resource.pure(blockHeaderFetcher))

      val blockBodyFetcher = mock[PeerBlockBodyFetcherActor[F]]
      (() => blockBodyFetcher.id).expects().anyNumberOfTimes().returns(2)
      (networkAlgebra.makePeerBodyFetcher _).expects(*, *, *, *, *, *).returns(Resource.pure(blockBodyFetcher))

      val mempoolSync = mock[PeerMempoolTransactionSyncActor[F]]
      (() => mempoolSync.id).expects().anyNumberOfTimes().returns(3)
      (networkAlgebra.makeMempoolSyncFetcher _).expects(*, *, *, *, *, *).returns(Resource.pure(mempoolSync))

      val pingDelay = FiniteDuration(10, MILLISECONDS)

      val client = mock[BlockchainPeerClient[F]]
      (client.notifyAboutThisNetworkLevel _).expects(true).returns(Applicative[F].unit)
      (client.getPongMessage _).expects(*).atLeastOnce().onCall { ping: PingMessage =>
        Async[F].delayBy(Option(PongMessage(ping.ping.reverse)).pure[F], pingDelay)
      }
      (client
        .getRemoteBlockIdAtHeight(_: Long, _: Option[BlockId]))
        .expects(1L, genesis.slotId.blockId.some)
        .once()
        .returning(genesis.slotId.blockId.some.pure[F])
      (client.notifyAboutThisNetworkLevel _).expects(false).returns(Applicative[F].unit)
      (client.closeConnection _).expects().returns(Applicative[F].unit)

      (peersManager.sendNoWait _).expects(*).atLeastOnce().onCall { message: PeersManager.Message =>
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
          peersManager,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          blockHeights,
          headerToBodyValidation,
          transactionSyntaxValidation,
          mempool
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
      val peersManager = mock[PeersManagerActor[F]]
      val requestsProxy = mock[RequestsProxyActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      (() => localChain.genesis).expects().once().returning(genesis.pure[F])
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val transactionStore = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree = mock[ParentChildTree[F, BlockId]]
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val networkAlgebra = mock[NetworkAlgebra[F]]
      val blockHeaderFetcher = mock[PeerBlockHeaderFetcherActor[F]]
      val transactionSyntaxValidation = mock[TransactionSyntaxVerifier[F]]
      val mempool = mock[MempoolAlgebra[F]]

      (() => blockHeaderFetcher.id).expects().anyNumberOfTimes().returns(1)
      (networkAlgebra.makePeerHeaderFetcher _).expects(*, *, *, *, *, *, *).returns(Resource.pure(blockHeaderFetcher))

      val blockBodyFetcher = mock[PeerBlockBodyFetcherActor[F]]
      (() => blockBodyFetcher.id).expects().anyNumberOfTimes().returns(2)
      (networkAlgebra.makePeerBodyFetcher _).expects(*, *, *, *, *, *).returns(Resource.pure(blockBodyFetcher))

      val mempoolSync = mock[PeerMempoolTransactionSyncActor[F]]
      (() => mempoolSync.id).expects().anyNumberOfTimes().returns(3)
      (networkAlgebra.makeMempoolSyncFetcher _).expects(*, *, *, *, *, *).returns(Resource.pure(mempoolSync))

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
      (client.notifyAboutThisNetworkLevel _).expects(false).returns(Applicative[F].unit)
      (client.closeConnection _).expects().returns(Applicative[F].unit)

      (peersManager.sendNoWait _).expects(*).once().onCall { message: PeersManager.Message =>
        message match {
          case PingPongMessagePing(`hostId`, Right(t)) =>
            assert(t >= pingDelay.toMillis)
            ().pure[F]
          case _ => throw new IllegalStateException()
        }
      }
      (peersManager.sendNoWait _).expects(*).once().onCall { message: PeersManager.Message =>
        message match {
          case PingPongMessagePing(`hostId`, Left(NoPongMessage)) => ().pure[F]
          case _                                                  => throw new IllegalStateException()
        }
      }
      (peersManager.sendNoWait _).expects(*).atLeastOnce().onCall { message: PeersManager.Message =>
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
          peersManager,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          blockHeights,
          headerToBodyValidation,
          transactionSyntaxValidation,
          mempool
        )
        .use { actor =>
          actor.send(PeerActor.Message.GetNetworkQuality) >>
          actor.send(PeerActor.Message.GetNetworkQuality) >>
          actor.send(PeerActor.Message.GetNetworkQuality)
        }
    }

  }

  test("Ping error shall be processed correctly") {
    withMock {
      val peersManager = mock[PeersManagerActor[F]]
      val requestsProxy = mock[RequestsProxyActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      (() => localChain.genesis).expects().once().returning(genesis.pure[F])
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val transactionStore = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree = mock[ParentChildTree[F, BlockId]]
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val networkAlgebra = mock[NetworkAlgebra[F]]
      val blockHeaderFetcher = mock[PeerBlockHeaderFetcherActor[F]]
      val transactionSyntaxValidation = mock[TransactionSyntaxVerifier[F]]
      val mempool = mock[MempoolAlgebra[F]]

      (() => blockHeaderFetcher.id).expects().anyNumberOfTimes().returns(1)
      (networkAlgebra.makePeerHeaderFetcher _).expects(*, *, *, *, *, *, *).returns(Resource.pure(blockHeaderFetcher))

      val blockBodyFetcher = mock[PeerBlockBodyFetcherActor[F]]
      (() => blockBodyFetcher.id).expects().anyNumberOfTimes().returns(2)
      (networkAlgebra.makePeerBodyFetcher _).expects(*, *, *, *, *, *).returns(Resource.pure(blockBodyFetcher))

      val mempoolSync = mock[PeerMempoolTransactionSyncActor[F]]
      (() => mempoolSync.id).expects().anyNumberOfTimes().returns(3)
      (networkAlgebra.makeMempoolSyncFetcher _).expects(*, *, *, *, *, *).returns(Resource.pure(mempoolSync))

      val client = mock[BlockchainPeerClient[F]]
      (client
        .getRemoteBlockIdAtHeight(_: Long, _: Option[BlockId]))
        .expects(1L, genesis.slotId.blockId.some)
        .once()
        .returning(genesis.slotId.blockId.some.pure[F])
      (client.getPongMessage _).expects(*).once().onCall { _: PingMessage => throw new RuntimeException() }
      (client.notifyAboutThisNetworkLevel _).expects(false).returns(Applicative[F].unit)
      (client.closeConnection _).expects().returns(Applicative[F].unit)

      (peersManager.sendNoWait _)
        .expects(PeersManager.Message.NonCriticalErrorForHost(hostId))
        .returns(Applicative[F].unit)

      PeerActor
        .makeActor(
          hostId,
          networkAlgebra,
          client,
          requestsProxy,
          peersManager,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          blockHeights,
          headerToBodyValidation,
          transactionSyntaxValidation,
          mempool
        )
        .use { actor =>
          actor.send(PeerActor.Message.GetNetworkQuality)
        }
    }

  }

  test("Common ancestor shall be tracked") {
    withMock {
      val peersManager = mock[PeersManagerActor[F]]
      val requestsProxy = mock[RequestsProxyActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      (() => localChain.genesis).expects().once().returning(genesis.pure[F])
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val transactionStore = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree = mock[ParentChildTree[F, BlockId]]
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val networkAlgebra = mock[NetworkAlgebra[F]]
      val blockHeaderFetcher = mock[PeerBlockHeaderFetcherActor[F]]
      val transactionSyntaxValidation = mock[TransactionSyntaxVerifier[F]]
      val mempool = mock[MempoolAlgebra[F]]
      val commonAncestorSlotData = arbitrarySlotData.arbitrary.first
      val commonAncestor = commonAncestorSlotData.slotId.blockId

      (() => blockHeaderFetcher.id).expects().anyNumberOfTimes().returns(1)
      (networkAlgebra.makePeerHeaderFetcher _).expects(*, *, *, *, *, *, *).returns(Resource.pure(blockHeaderFetcher))

      val blockBodyFetcher = mock[PeerBlockBodyFetcherActor[F]]
      (() => blockBodyFetcher.id).expects().anyNumberOfTimes().returns(2)
      (networkAlgebra.makePeerBodyFetcher _).expects(*, *, *, *, *, *).returns(Resource.pure(blockBodyFetcher))

      val mempoolSync = mock[PeerMempoolTransactionSyncActor[F]]
      (() => mempoolSync.id).expects().anyNumberOfTimes().returns(3)
      (networkAlgebra.makeMempoolSyncFetcher _).expects(*, *, *, *, *, *).returns(Resource.pure(mempoolSync))

      val client = mock[BlockchainPeerClient[F]]
      (client
        .findCommonAncestor(_: Long => F[BlockId], _: () => F[Long])(_: Sync[F], _: Logger[F]))
        .expects(*, *, *, *)
        .once()
        .returns(commonAncestor.pure[F])

      (slotDataStore
        .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
        .expects(commonAncestor, *, *)
        .returns(commonAncestorSlotData.pure[F])

      (client
        .getRemoteBlockIdAtHeight(_: Long, _: Option[BlockId]))
        .expects(1L, genesis.slotId.blockId.some)
        .once()
        .returning(genesis.slotId.blockId.some.pure[F])
      (client.notifyAboutThisNetworkLevel _).expects(false).returns(Applicative[F].unit)
      (client.closeConnection _).expects().returns(Applicative[F].unit)

      PeerActor
        .makeActor(
          hostId,
          networkAlgebra,
          client,
          requestsProxy,
          peersManager,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          blockHeights,
          headerToBodyValidation,
          transactionSyntaxValidation,
          mempool
        )
        .use { actor =>
          actor.send(PeerActor.Message.PrintCommonAncestor)
        }
    }

  }

  test("Common ancestor error shall be processed") {
    withMock {
      val peersManager = mock[PeersManagerActor[F]]
      val requestsProxy = mock[RequestsProxyActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      (() => localChain.genesis).expects().once().returning(genesis.pure[F])
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val transactionStore = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree = mock[ParentChildTree[F, BlockId]]
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val networkAlgebra = mock[NetworkAlgebra[F]]
      val blockHeaderFetcher = mock[PeerBlockHeaderFetcherActor[F]]
      val transactionSyntaxValidation = mock[TransactionSyntaxVerifier[F]]
      val mempool = mock[MempoolAlgebra[F]]
      val commonAncestorSlotData = arbitrarySlotData.arbitrary.first
      val commonAncestor = commonAncestorSlotData.slotId.blockId

      (() => blockHeaderFetcher.id).expects().anyNumberOfTimes().returns(1)
      (networkAlgebra.makePeerHeaderFetcher _).expects(*, *, *, *, *, *, *).returns(Resource.pure(blockHeaderFetcher))

      val blockBodyFetcher = mock[PeerBlockBodyFetcherActor[F]]
      (() => blockBodyFetcher.id).expects().anyNumberOfTimes().returns(2)
      (networkAlgebra.makePeerBodyFetcher _).expects(*, *, *, *, *, *).returns(Resource.pure(blockBodyFetcher))

      val mempoolSync = mock[PeerMempoolTransactionSyncActor[F]]
      (() => mempoolSync.id).expects().anyNumberOfTimes().returns(3)
      (networkAlgebra.makeMempoolSyncFetcher _).expects(*, *, *, *, *, *).returns(Resource.pure(mempoolSync))

      val client = mock[BlockchainPeerClient[F]]
      (client
        .findCommonAncestor(_: Long => F[BlockId], _: () => F[Long])(_: Sync[F], _: Logger[F]))
        .expects(*, *, *, *)
        .once()
        .returns(Sync[F].delay(throw new RuntimeException()))

      (slotDataStore
        .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
        .expects(commonAncestor, *, *)
        .never()
        .returns(commonAncestorSlotData.pure[F])

      (client
        .getRemoteBlockIdAtHeight(_: Long, _: Option[BlockId]))
        .expects(1L, genesis.slotId.blockId.some)
        .once()
        .returning(genesis.slotId.blockId.some.pure[F])
      (client.notifyAboutThisNetworkLevel _).expects(false).returns(Applicative[F].unit)
      (client.closeConnection _).expects().returns(Applicative[F].unit)

      (peersManager.sendNoWait _)
        .expects(PeersManager.Message.NonCriticalErrorForHost(hostId))
        .returns(Applicative[F].unit)

      PeerActor
        .makeActor(
          hostId,
          networkAlgebra,
          client,
          requestsProxy,
          peersManager,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          blockHeights,
          headerToBodyValidation,
          transactionSyntaxValidation,
          mempool
        )
        .use { actor =>
          actor.send(PeerActor.Message.PrintCommonAncestor)
        }
    }

  }

  test("Request to get current tip shall be forwarded to block header fetcher") {
    withMock {
      val peersManager = mock[PeersManagerActor[F]]
      val requestsProxy = mock[RequestsProxyActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      (() => localChain.genesis).expects().once().returning(genesis.pure[F])
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val transactionStore = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree = mock[ParentChildTree[F, BlockId]]
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val client = createDummyClient()
      val networkAlgebra = mock[NetworkAlgebra[F]]
      val blockHeaderFetcher = mock[PeerBlockHeaderFetcherActor[F]]
      val transactionSyntaxValidation = mock[TransactionSyntaxVerifier[F]]
      val mempool = mock[MempoolAlgebra[F]]

      (() => blockHeaderFetcher.id).expects().anyNumberOfTimes().returns(1)
      (networkAlgebra.makePeerHeaderFetcher _).expects(*, *, *, *, *, *, *).returns(Resource.pure(blockHeaderFetcher))

      val blockBodyFetcher = mock[PeerBlockBodyFetcherActor[F]]
      (() => blockBodyFetcher.id).expects().anyNumberOfTimes().returns(2)
      (networkAlgebra.makePeerBodyFetcher _).expects(*, *, *, *, *, *).returns(Resource.pure(blockBodyFetcher))

      val mempoolSync = mock[PeerMempoolTransactionSyncActor[F]]
      (() => mempoolSync.id).expects().anyNumberOfTimes().returns(3)
      (networkAlgebra.makeMempoolSyncFetcher _).expects(*, *, *, *, *, *).returns(Resource.pure(mempoolSync))

      (blockHeaderFetcher.sendNoWait _)
        .expects(PeerBlockHeaderFetcher.Message.GetCurrentTip)
        .returns(Applicative[F].unit)

      PeerActor
        .makeActor(
          hostId,
          networkAlgebra,
          client,
          requestsProxy,
          peersManager,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          blockHeights,
          headerToBodyValidation,
          transactionSyntaxValidation,
          mempool
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
      val peersManager = mock[PeersManagerActor[F]]
      val requestsProxy = mock[RequestsProxyActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      (() => localChain.genesis).expects().once().returning(genesis.pure[F])
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val transactionStore = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree = mock[ParentChildTree[F, BlockId]]
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val client = createDummyClient()
      val networkAlgebra = mock[NetworkAlgebra[F]]
      val blockHeaderFetcher = mock[PeerBlockHeaderFetcherActor[F]]
      val transactionSyntaxValidation = mock[TransactionSyntaxVerifier[F]]
      val mempool = mock[MempoolAlgebra[F]]

      (() => blockHeaderFetcher.id).expects().anyNumberOfTimes().returns(1)
      (networkAlgebra.makePeerHeaderFetcher _).expects(*, *, *, *, *, *, *).returns(Resource.pure(blockHeaderFetcher))

      val blockBodyFetcher = mock[PeerBlockBodyFetcherActor[F]]
      (() => blockBodyFetcher.id).expects().anyNumberOfTimes().returns(2)
      (networkAlgebra.makePeerBodyFetcher _).expects(*, *, *, *, *, *).returns(Resource.pure(blockBodyFetcher))

      val mempoolSync = mock[PeerMempoolTransactionSyncActor[F]]
      (() => mempoolSync.id).expects().anyNumberOfTimes().returns(3)
      (networkAlgebra.makeMempoolSyncFetcher _).expects(*, *, *, *, *, *).returns(Resource.pure(mempoolSync))

      val host1 = KnownHost("0.0.0.1", 1)
      val host2 = KnownHost("0.0.0.2", 2)
      val hosts = Seq(host1, host2)
      (client.getRemoteKnownHosts _)
        .expects(CurrentKnownHostsReq(2))
        .returns(Option(CurrentKnownHostsRes(hosts)).pure[F])

      (peersManager.sendNoWait _)
        .expects(
          PeersManager.Message.AddKnownNeighbors(hostId, NonEmptyChain.fromSeq(hosts.map(_.asRemoteAddress)).get)
        )
        .returns(Applicative[F].unit)

      PeerActor
        .makeActor(
          hostId,
          networkAlgebra,
          client,
          requestsProxy,
          peersManager,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          blockHeights,
          headerToBodyValidation,
          transactionSyntaxValidation,
          mempool
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
      val peersManager = mock[PeersManagerActor[F]]
      val requestsProxy = mock[RequestsProxyActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      (() => localChain.genesis).expects().once().returning(genesis.pure[F])
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val transactionStore = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree = mock[ParentChildTree[F, BlockId]]
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val client = createDummyClient()
      val networkAlgebra = mock[NetworkAlgebra[F]]
      val blockHeaderFetcher = mock[PeerBlockHeaderFetcherActor[F]]
      val transactionSyntaxValidation = mock[TransactionSyntaxVerifier[F]]
      val mempool = mock[MempoolAlgebra[F]]

      (() => blockHeaderFetcher.id).expects().anyNumberOfTimes().returns(1)
      (networkAlgebra.makePeerHeaderFetcher _).expects(*, *, *, *, *, *, *).returns(Resource.pure(blockHeaderFetcher))

      val blockBodyFetcher = mock[PeerBlockBodyFetcherActor[F]]
      (() => blockBodyFetcher.id).expects().anyNumberOfTimes().returns(2)
      (networkAlgebra.makePeerBodyFetcher _).expects(*, *, *, *, *, *).returns(Resource.pure(blockBodyFetcher))

      val mempoolSync = mock[PeerMempoolTransactionSyncActor[F]]
      (() => mempoolSync.id).expects().anyNumberOfTimes().returns(3)
      (networkAlgebra.makeMempoolSyncFetcher _).expects(*, *, *, *, *, *).returns(Resource.pure(mempoolSync))

      (client.getRemoteKnownHosts _)
        .expects(CurrentKnownHostsReq(2))
        .returns(Option(CurrentKnownHostsRes()).pure[F])

      PeerActor
        .makeActor(
          hostId,
          networkAlgebra,
          client,
          requestsProxy,
          peersManager,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          blockHeights,
          headerToBodyValidation,
          transactionSyntaxValidation,
          mempool
        )
        .use { actor =>
          for {
            _ <- actor.send(PeerActor.Message.GetHotPeersFromPeer(2))
          } yield ()
        }
    }
  }

  test("Error of request to get peer neighbours shall be processed") {
    withMock {
      val peersManager = mock[PeersManagerActor[F]]
      val requestsProxy = mock[RequestsProxyActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      (() => localChain.genesis).expects().once().returning(genesis.pure[F])
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val transactionStore = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree = mock[ParentChildTree[F, BlockId]]
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val client = createDummyClient()
      val networkAlgebra = mock[NetworkAlgebra[F]]
      val blockHeaderFetcher = mock[PeerBlockHeaderFetcherActor[F]]
      val transactionSyntaxValidation = mock[TransactionSyntaxVerifier[F]]
      val mempool = mock[MempoolAlgebra[F]]

      (() => blockHeaderFetcher.id).expects().anyNumberOfTimes().returns(1)
      (networkAlgebra.makePeerHeaderFetcher _).expects(*, *, *, *, *, *, *).returns(Resource.pure(blockHeaderFetcher))

      val blockBodyFetcher = mock[PeerBlockBodyFetcherActor[F]]
      (() => blockBodyFetcher.id).expects().anyNumberOfTimes().returns(2)
      (networkAlgebra.makePeerBodyFetcher _).expects(*, *, *, *, *, *).returns(Resource.pure(blockBodyFetcher))

      val mempoolSync = mock[PeerMempoolTransactionSyncActor[F]]
      (() => mempoolSync.id).expects().anyNumberOfTimes().returns(3)
      (networkAlgebra.makeMempoolSyncFetcher _).expects(*, *, *, *, *, *).returns(Resource.pure(mempoolSync))

      (client.getRemoteKnownHosts _)
        .expects(CurrentKnownHostsReq(2))
        .onCall { _: CurrentKnownHostsReq => throw new RuntimeException() }

      (peersManager.sendNoWait _)
        .expects(PeersManager.Message.NonCriticalErrorForHost(hostId))
        .returns(Applicative[F].unit)

      PeerActor
        .makeActor(
          hostId,
          networkAlgebra,
          client,
          requestsProxy,
          peersManager,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          blockHeights,
          headerToBodyValidation,
          transactionSyntaxValidation,
          mempool
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
      val peersManager = mock[PeersManagerActor[F]]
      val requestsProxy = mock[RequestsProxyActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      (() => localChain.genesis).expects().once().returning(genesis.pure[F])
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val transactionStore = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree = mock[ParentChildTree[F, BlockId]]
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val client = createDummyClient()
      val networkAlgebra = mock[NetworkAlgebra[F]]
      val blockHeaderFetcher = mock[PeerBlockHeaderFetcherActor[F]]
      val transactionSyntaxValidation = mock[TransactionSyntaxVerifier[F]]
      val mempool = mock[MempoolAlgebra[F]]

      (() => blockHeaderFetcher.id).expects().anyNumberOfTimes().returns(1)
      (networkAlgebra.makePeerHeaderFetcher _).expects(*, *, *, *, *, *, *).returns(Resource.pure(blockHeaderFetcher))

      val blockBodyFetcher = mock[PeerBlockBodyFetcherActor[F]]
      (() => blockBodyFetcher.id).expects().anyNumberOfTimes().returns(2)
      (networkAlgebra.makePeerBodyFetcher _).expects(*, *, *, *, *, *).returns(Resource.pure(blockBodyFetcher))

      val mempoolSync = mock[PeerMempoolTransactionSyncActor[F]]
      (() => mempoolSync.id).expects().anyNumberOfTimes().returns(3)
      (networkAlgebra.makeMempoolSyncFetcher _).expects(*, *, *, *, *, *).returns(Resource.pure(mempoolSync))

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
          peersManager,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          blockHeights,
          headerToBodyValidation,
          transactionSyntaxValidation,
          mempool
        )
        .use { actor =>
          for {
            _ <- actor.send(PeerActor.Message.GetPeerServerAddress)
          } yield ()
        }
    }
  }

  test("Request to get server port do nothing if no server port is returned") {
    withMock {
      val peersManager = mock[PeersManagerActor[F]]
      val requestsProxy = mock[RequestsProxyActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      (() => localChain.genesis).expects().once().returning(genesis.pure[F])
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val transactionStore = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree = mock[ParentChildTree[F, BlockId]]
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val client = createDummyClient()
      val networkAlgebra = mock[NetworkAlgebra[F]]
      val blockHeaderFetcher = mock[PeerBlockHeaderFetcherActor[F]]
      val transactionSyntaxValidation = mock[TransactionSyntaxVerifier[F]]
      val mempool = mock[MempoolAlgebra[F]]

      (() => blockHeaderFetcher.id).expects().anyNumberOfTimes().returns(1)
      (networkAlgebra.makePeerHeaderFetcher _).expects(*, *, *, *, *, *, *).returns(Resource.pure(blockHeaderFetcher))

      val blockBodyFetcher = mock[PeerBlockBodyFetcherActor[F]]
      (() => blockBodyFetcher.id).expects().anyNumberOfTimes().returns(2)
      (networkAlgebra.makePeerBodyFetcher _).expects(*, *, *, *, *, *).returns(Resource.pure(blockBodyFetcher))

      val mempoolSync = mock[PeerMempoolTransactionSyncActor[F]]
      (() => mempoolSync.id).expects().anyNumberOfTimes().returns(3)
      (networkAlgebra.makeMempoolSyncFetcher _).expects(*, *, *, *, *, *).returns(Resource.pure(mempoolSync))

      val serverPort = 9085
      (() => client.remotePeerServerPort)
        .expects()
        .returns(None.pure[F])

      (peersManager.sendNoWait _)
        .expects(PeersManager.Message.RemotePeerServerPort(hostId, serverPort))
        .never()
        .returns(Applicative[F].unit)

      PeerActor
        .makeActor(
          hostId,
          networkAlgebra,
          client,
          requestsProxy,
          peersManager,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          blockHeights,
          headerToBodyValidation,
          transactionSyntaxValidation,
          mempool
        )
        .use { actor =>
          for {
            _ <- actor.send(PeerActor.Message.GetPeerServerAddress)
          } yield ()
        }
    }
  }

  test("Error during requesting of getting server port shall be processed") {
    withMock {
      val peersManager = mock[PeersManagerActor[F]]
      val requestsProxy = mock[RequestsProxyActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      (() => localChain.genesis).expects().once().returning(genesis.pure[F])
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val transactionStore = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree = mock[ParentChildTree[F, BlockId]]
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val client = createDummyClient()
      val networkAlgebra = mock[NetworkAlgebra[F]]
      val blockHeaderFetcher = mock[PeerBlockHeaderFetcherActor[F]]
      val transactionSyntaxValidation = mock[TransactionSyntaxVerifier[F]]
      val mempool = mock[MempoolAlgebra[F]]

      (() => blockHeaderFetcher.id).expects().anyNumberOfTimes().returns(1)
      (networkAlgebra.makePeerHeaderFetcher _).expects(*, *, *, *, *, *, *).returns(Resource.pure(blockHeaderFetcher))

      val blockBodyFetcher = mock[PeerBlockBodyFetcherActor[F]]
      (() => blockBodyFetcher.id).expects().anyNumberOfTimes().returns(2)
      (networkAlgebra.makePeerBodyFetcher _).expects(*, *, *, *, *, *).returns(Resource.pure(blockBodyFetcher))

      val mempoolSync = mock[PeerMempoolTransactionSyncActor[F]]
      (() => mempoolSync.id).expects().anyNumberOfTimes().returns(3)
      (networkAlgebra.makeMempoolSyncFetcher _).expects(*, *, *, *, *, *).returns(Resource.pure(mempoolSync))

      (() => client.remotePeerServerPort)
        .expects()
        .onCall(() => throw new RuntimeException())

      (peersManager.sendNoWait _)
        .expects(PeersManager.Message.NonCriticalErrorForHost(hostId))
        .returns(Applicative[F].unit)

      PeerActor
        .makeActor(
          hostId,
          networkAlgebra,
          client,
          requestsProxy,
          peersManager,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          blockHeights,
          headerToBodyValidation,
          transactionSyntaxValidation,
          mempool
        )
        .use { actor =>
          for {
            _ <- actor.send(PeerActor.Message.GetPeerServerAddress)
          } yield ()
        }
    }
  }

  test("Mismatched genesis block should result non critical error message to reputation aggregator") {
    withMock {
      val localGenesis = arbitrarySlotData.arbitrary.first
      val remoteGenesisId = arbitraryBlockId.arbitrary.first
      val peersManager = mock[PeersManagerActor[F]]
      val requestsProxy = mock[RequestsProxyActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      (() => localChain.genesis).expects().once().returning(localGenesis.pure[F])
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val transactionStore = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree = mock[ParentChildTree[F, BlockId]]
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val transactionSyntaxValidation = mock[TransactionSyntaxVerifier[F]]
      val mempool = mock[MempoolAlgebra[F]]

      val client = mock[BlockchainPeerClient[F]]
      (client.notifyAboutThisNetworkLevel _).stubs(*).returns(Applicative[F].unit)
      (client
        .getRemoteBlockIdAtHeight(_: Long, _: Option[BlockId]))
        .expects(1L, localGenesis.slotId.blockId.some)
        .once()
        .returning(remoteGenesisId.some.pure[F])
      (client.closeConnection _).stubs().returns(Applicative[F].unit)
      (peersManager.sendNoWait _)
        .expects(PeersManager.Message.NonCriticalErrorForHost(hostId))
        .once()
        .returns(Applicative[F].unit)

      val networkAlgebra = mock[NetworkAlgebra[F]]
      val blockHeaderFetcher = mock[PeerBlockHeaderFetcherActor[F]]
      (networkAlgebra.makePeerHeaderFetcher _).expects(*, *, *, *, *, *, *).returns(Resource.pure(blockHeaderFetcher))

      val blockBodyFetcher = mock[PeerBlockBodyFetcherActor[F]]
      (networkAlgebra.makePeerBodyFetcher _).expects(*, *, *, *, *, *).returns(Resource.pure(blockBodyFetcher))

      val mempoolSync = mock[PeerMempoolTransactionSyncActor[F]]
      (() => mempoolSync.id).expects().anyNumberOfTimes().returns(3)
      (networkAlgebra.makeMempoolSyncFetcher _).expects(*, *, *, *, *, *).returns(Resource.pure(mempoolSync))

      PeerActor
        .makeActor(
          hostId,
          networkAlgebra,
          client,
          requestsProxy,
          peersManager,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          blockHeights,
          headerToBodyValidation,
          transactionSyntaxValidation,
          mempool
        )
        .use(_ => Applicative[F].unit)
    }
  }

  test("Finalizer shall call appropriate calls to client") {
    withMock {
      arbitrarySlotData.arbitrary.first
      val peersManager = mock[PeersManagerActor[F]]
      val requestsProxy = mock[RequestsProxyActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      (() => localChain.genesis).expects().once().returning(genesis.pure[F])
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val transactionStore = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree = mock[ParentChildTree[F, BlockId]]
      val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
      val headerToBodyValidation = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val client = mock[BlockchainPeerClient[F]]
      val transactionSyntaxValidation = mock[TransactionSyntaxVerifier[F]]
      val mempool = mock[MempoolAlgebra[F]]

      val (networkAlgebra, _, _, _) = createDummyNetworkAlgebra()
      (client.notifyAboutThisNetworkLevel _).expects(true).returns(Applicative[F].unit)
      (client
        .getRemoteBlockIdAtHeight(_: Long, _: Option[BlockId]))
        .expects(1L, genesis.slotId.blockId.some)
        .once()
        .returning(genesis.slotId.blockId.some.pure[F])
      (client.getPongMessage _).stubs(*).onCall { ping: PingMessage =>
        Option(PongMessage(ping.ping.reverse)).pure[F]
      }
      (peersManager.sendNoWait _).stubs(*).returns(Applicative[F].unit)
      (client.closeConnection _).stubs().returns(Applicative[F].unit)
      (client.notifyAboutThisNetworkLevel _).expects(false).returns(Applicative[F].unit)

      PeerActor
        .makeActor(
          hostId,
          networkAlgebra,
          client,
          requestsProxy,
          peersManager,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          blockHeights,
          headerToBodyValidation,
          transactionSyntaxValidation,
          mempool
        )
        .use { actor =>
          for {
            _ <- actor.send(PeerActor.Message.UpdateState(networkLevel = true, applicationLevel = true))
          } yield ()
        }
    }
  }
}
