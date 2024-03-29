package co.topl.networking.fsnetwork

import cats.{Applicative, MonadThrow, Show}
import cats.data.{NonEmptyChain, OptionT}
import cats.effect.kernel.Sync
import cats.effect.{Async, IO}
import cats.implicits._
import co.topl.algebras.{ClockAlgebra, Store}
import co.topl.codecs.bytes.tetra.instances._
import co.topl.consensus.algebras.{ChainSelectionAlgebra, LocalChainAlgebra}
import co.topl.consensus.models.{BlockHeader, BlockId, SlotData}
import co.topl.eventtree.ParentChildTree
import co.topl.models.ModelGenerators.GenHelper
import co.topl.models.generators.consensus.ModelGenerators._
import co.topl.networking.blockchain.BlockchainPeerClient
import co.topl.networking.fsnetwork.BlockDownloadError.BlockHeaderDownloadError
import co.topl.networking.fsnetwork.BlockDownloadError.BlockHeaderDownloadError._
import co.topl.networking.fsnetwork.PeerBlockHeaderFetcherTest.{BlockHeaderDownloadErrorByName, F}
import co.topl.networking.fsnetwork.PeersManager.PeersManagerActor
import co.topl.networking.fsnetwork.RequestsProxy.RequestsProxyActor
import co.topl.networking.fsnetwork.TestHelper._
import co.topl.node.models.BlockBody
import fs2.Stream
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.Gen
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.collection.mutable
import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}

object PeerBlockHeaderFetcherTest {
  type F[A] = IO[A]
  type BlockHeaderDownloadErrorByName = () => BlockHeaderDownloadError
}

class PeerBlockHeaderFetcherTest extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {
  implicit val logger: Logger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)

  val hostId: HostId = arbitraryHost.arbitrary.first
  val maxChainSize = 99

  val defaultChainSelectionAlgebra: ChainSelectionAlgebra[F, SlotData] = new ChainSelectionAlgebra[F, SlotData] {
    override def compare(x: SlotData, y: SlotData): F[Int] = x.height.compare(y.height).pure[F]

    override def enoughHeightToCompare(currentHeight: Long, commonHeight: Long, proposedHeight: Long): F[Long] =
      proposedHeight.pure[F]
  }

  def defaultBodyStorage: Store[F, BlockId, BlockBody] = mock[Store[F, BlockId, BlockBody]]

  def buildLocalChainAlgebra(): LocalChainAlgebra[F] = {
    val localChainAlgebra = mock[LocalChainAlgebra[F]]
    (() => localChainAlgebra.chainSelectionAlgebra).stubs().returning(defaultChainSelectionAlgebra.pure[F])
    localChainAlgebra
  }

  test("Block header shall be downloaded by request") {
    withMock {
      val peersManager = mock[PeersManagerActor[F]]
      val headers: NonEmptyChain[BlockHeader] =
        nonEmptyChainArbOf(arbitraryHeader).arbitrary.retryUntil(_.size < maxChainSize).first.map(_.embedId)
      val idAndHeader: NonEmptyChain[(BlockId, BlockHeader)] =
        headers.map(h => (h.id, h))
      val idToHeader: Map[BlockId, BlockHeader] = idAndHeader.toList.toMap
      val blocksCount = idToHeader.size

      val client = mock[BlockchainPeerClient[F]]
      (client
        .getHeaderOrError(_: BlockId, _: Throwable)(_: MonadThrow[F]))
        .expects(*, *, *)
        .rep(blocksCount)
        .onCall { case (id: BlockId, e: BlockHeaderDownloadErrorByName @unchecked, _: MonadThrow[F] @unchecked) =>
          OptionT(idToHeader.get(id).pure[F]).getOrRaise(e.apply())
        }

      val requestsProxy = mock[RequestsProxyActor[F]]
      val expectedMessage: RequestsProxy.Message =
        RequestsProxy.Message.DownloadHeadersResponse(
          hostId,
          idAndHeader.map { case (id, header) => (id, Either.right(UnverifiedBlockHeader(hostId, header, 0))) }
        )
      (requestsProxy.sendNoWait _)
        .expects(compareDownloadedHeaderWithoutDownloadTimeMatcher(expectedMessage))
        .once()
        .returning(().pure[F])

      val localChain = buildLocalChainAlgebra()

      val slotDataStore = mock[Store[F, BlockId, SlotData]]

      val blockIdTree = mock[ParentChildTree[F, BlockId]]

      val clock = mock[ClockAlgebra[F]]

      val blockHeights = mock[BlockHeights[F]]

      val commonAncestorF = mock[(BlockchainPeerClient[F], BlockHeights[F], LocalChainAlgebra[F]) => F[BlockId]]

      PeerBlockHeaderFetcher
        .makeActor(
          hostId,
          client,
          requestsProxy,
          peersManager,
          localChain,
          slotDataStore,
          defaultBodyStorage,
          blockIdTree,
          clock,
          blockHeights,
          commonAncestorF
        )
        .use { actor =>
          for {
            _ <- actor.sendNoWait(PeerBlockHeaderFetcher.Message.DownloadBlockHeaders(idAndHeader.map(_._1)))
          } yield ()
        }
    }
  }

  test("One block header shall be downloaded by request proper with delay measurements") {
    withMock {
      val pingDelay = 90

      val peersManager = mock[PeersManagerActor[F]]

      val header: BlockHeader = arbitraryHeader.arbitrary.first.embedId
      val client = mock[BlockchainPeerClient[F]]
      (client
        .getHeaderOrError(_: BlockId, _: Throwable)(_: MonadThrow[F]))
        .expects(header.id, *, *)
        .once()
        .returns(Async[F].delayBy(header.pure[F], FiniteDuration(pingDelay, MILLISECONDS)))

      val requestsProxy = mock[RequestsProxyActor[F]]

      (requestsProxy.sendNoWait _)
        .expects(*)
        .once()
        .onCall { message: RequestsProxy.Message =>
          message match {
            case RequestsProxy.Message.DownloadHeadersResponse(`hostId`, response) =>
              if (response.head._2.forall(b => b.downloadTimeMs < pingDelay || b.downloadTimeMs > pingDelay + 30))
                throw new IllegalStateException()
              else
                ().pure[F]
            case _ => throw new IllegalStateException()
          }
        }

      val localChain = buildLocalChainAlgebra()

      val slotDataStore = mock[Store[F, BlockId, SlotData]]

      val blockIdTree = mock[ParentChildTree[F, BlockId]]

      val clock = mock[ClockAlgebra[F]]

      val blockHeights = mock[BlockHeights[F]]

      val commonAncestorF = mock[(BlockchainPeerClient[F], BlockHeights[F], LocalChainAlgebra[F]) => F[BlockId]]

      PeerBlockHeaderFetcher
        .makeActor(
          hostId,
          client,
          requestsProxy,
          peersManager,
          localChain,
          slotDataStore,
          defaultBodyStorage,
          blockIdTree,
          clock,
          blockHeights,
          commonAncestorF
        )
        .use { actor =>
          for {
            _ <- actor.sendNoWait(PeerBlockHeaderFetcher.Message.DownloadBlockHeaders(NonEmptyChain.one(header.id)))
          } yield ()
        }
    }
  }

  test("Block header shall be downloaded by request, for missing headers error shall be returned") {
    withMock {
      val peersManager = mock[PeersManagerActor[F]]

      val headers: NonEmptyChain[BlockHeader] =
        nonEmptyChainArbOf(arbitraryHeader).arbitrary.retryUntil(_.size < maxChainSize).first.map(_.embedId)
      val idAndHeader: NonEmptyChain[(BlockId, BlockHeader)] =
        headers.map(h => (h.id, h))

      def missedBlockId(id: BlockId): Boolean = (id.hashCode() % 2) == 0

      val idToHeaderOnClient: Map[BlockId, BlockHeader] = idAndHeader.toList.filterNot(d => missedBlockId(d._1)).toMap
      val client = mock[BlockchainPeerClient[F]]
      (client
        .getHeaderOrError(_: BlockId, _: Throwable)(_: MonadThrow[F]))
        .expects(*, *, *)
        .rep(idAndHeader.size.toInt)
        .onCall { case (id: BlockId, e: BlockHeaderDownloadErrorByName @unchecked, _: MonadThrow[F] @unchecked) =>
          OptionT(idToHeaderOnClient.get(id).pure[F]).getOrRaise(e.apply())
        }

      val requestsProxy = mock[RequestsProxyActor[F]]
      val expectedMessage: RequestsProxy.Message =
        RequestsProxy.Message.DownloadHeadersResponse(
          hostId,
          idAndHeader.map { case (id, header) =>
            if (missedBlockId(id)) {
              (id, Either.left(HeaderNotFoundInPeer))
            } else {
              (id, Either.right(UnverifiedBlockHeader(hostId, header, 0)))
            }
          }
        )
      (requestsProxy.sendNoWait _)
        .expects(compareDownloadedHeaderWithoutDownloadTimeMatcher(expectedMessage))
        .once()
        .returning(().pure[F])

      val localChain = buildLocalChainAlgebra()

      val slotDataStore = mock[Store[F, BlockId, SlotData]]

      val blockIdTree = mock[ParentChildTree[F, BlockId]]

      val clock = mock[ClockAlgebra[F]]

      val blockHeights = mock[BlockHeights[F]]

      val commonAncestorF = mock[(BlockchainPeerClient[F], BlockHeights[F], LocalChainAlgebra[F]) => F[BlockId]]

      PeerBlockHeaderFetcher
        .makeActor(
          hostId,
          client,
          requestsProxy,
          peersManager,
          localChain,
          slotDataStore,
          defaultBodyStorage,
          blockIdTree,
          clock,
          blockHeights,
          commonAncestorF
        )
        .use { actor =>
          for {
            _ <- actor.sendNoWait(PeerBlockHeaderFetcher.Message.DownloadBlockHeaders(idAndHeader.map(_._1)))
          } yield ()
        }
    }
  }

  test("Block header shall be downloaded by request, incorrect headers shall be skipped") {
    withMock {
      val peersManager = mock[PeersManagerActor[F]]

      val headers: NonEmptyChain[BlockHeader] =
        nonEmptyChainArbOf(arbitraryHeader).arbitrary.retryUntil(_.size < maxChainSize).first.map(_.embedId)
      val idAndHeader: NonEmptyChain[(BlockId, BlockHeader)] =
        headers.map(h => (h.id, h))

      def missedBlockId(id: BlockId): Boolean = (id.hashCode() % 2) == 0

      val idToHeaderOnClient: Map[BlockId, BlockHeader] = idAndHeader.toList.filterNot(d => missedBlockId(d._1)).toMap
      val client = mock[BlockchainPeerClient[F]]
      val incorrectHeader = arbitraryHeader.arbitrary.first
      val incorrectHeaderId = incorrectHeader.id
      (client
        .getHeaderOrError(_: BlockId, _: Throwable)(_: MonadThrow[F]))
        .expects(*, *, *)
        .rep(idAndHeader.size.toInt)
        .onCall { case (id: BlockId, e: BlockHeaderDownloadErrorByName @unchecked, _: MonadThrow[F] @unchecked) =>
          if (missedBlockId(id)) {
            incorrectHeader.pure[F]
          } else {
            OptionT(idToHeaderOnClient.get(id).pure[F]).getOrRaise(e.apply())
          }
        }

      val requestsProxy = mock[RequestsProxyActor[F]]
      val expectedMessage: RequestsProxy.Message =
        RequestsProxy.Message.DownloadHeadersResponse(
          hostId,
          idAndHeader.map { case (id, header) =>
            if (missedBlockId(id)) {
              (id, Either.left(HeaderHaveIncorrectId(id, incorrectHeaderId)))
            } else {
              (id, Either.right(UnverifiedBlockHeader(hostId, header, 0)))
            }
          }
        )
      (requestsProxy.sendNoWait _)
        .expects(compareDownloadedHeaderWithoutDownloadTimeMatcher(expectedMessage))
        .once()
        .returning(().pure[F])

      val localChain = buildLocalChainAlgebra()

      val slotDataStore = mock[Store[F, BlockId, SlotData]]

      val blockIdTree = mock[ParentChildTree[F, BlockId]]

      val clock = mock[ClockAlgebra[F]]

      val blockHeights = mock[BlockHeights[F]]

      val commonAncestorF = mock[(BlockchainPeerClient[F], BlockHeights[F], LocalChainAlgebra[F]) => F[BlockId]]

      PeerBlockHeaderFetcher
        .makeActor(
          hostId,
          client,
          requestsProxy,
          peersManager,
          localChain,
          slotDataStore,
          defaultBodyStorage,
          blockIdTree,
          clock,
          blockHeights,
          commonAncestorF
        )
        .use { actor =>
          for {
            _ <- actor.sendNoWait(PeerBlockHeaderFetcher.Message.DownloadBlockHeaders(idAndHeader.map(_._1)))
          } yield ()
        }
    }
  }

  test("New better slot data and block source shall be sent if local chain is worse") {
    withMock {
      val slotData: NonEmptyChain[SlotData] =
        arbitraryLinkedSlotDataChainFor(Gen.choose(2, maxChainSize)).arbitrary.first
      val idAndSlotData: NonEmptyChain[(BlockId, SlotData)] = slotData.map(s => (s.slotId.blockId, s))
      val (knownId, knownSlotData) = idAndSlotData.head
      val remoteSlotData = NonEmptyChain.fromChain(idAndSlotData.tail).get

      val (bestSlotId, bestSlotData) = remoteSlotData.last
      val remoteIdToSlotData: Map[BlockId, SlotData] = remoteSlotData.toList.toMap
      val remoteSlotDataCount = remoteIdToSlotData.size

      val client = mock[BlockchainPeerClient[F]]
      (client
        .getSlotDataOrError(_: BlockId, _: Throwable)(_: MonadThrow[F]))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, e: BlockHeaderDownloadErrorByName @unchecked, _: MonadThrow[F] @unchecked) =>
          OptionT(remoteIdToSlotData.get(id).pure[F]).getOrRaise(e.apply())
        }
      (() => client.remotePeerAdoptions).expects().once().onCall { () =>
        Stream.eval[F, BlockId](bestSlotId.pure[F]).pure[F]
      }
      val remoteHeightToBlockId = remoteIdToSlotData.map { case (id, sd) => sd.height -> id }
      (client.getRemoteBlockIdAtHeight _).expects(*, *).onCall { case (height: Long, _: Option[BlockId] @unchecked) =>
        remoteHeightToBlockId.get(height).pure[F]
      }

      val peersManager = mock[PeersManagerActor[F]]
      val requestsProxy = mock[RequestsProxyActor[F]]

      val expectedSourceMessage: PeersManager.Message =
        PeersManager.Message.BlocksSource(remoteSlotData.map(d => (hostId, d._1)))
      (peersManager.sendNoWait _).expects(expectedSourceMessage).once().returning(().pure[F])
      val expectedSlotDataMessage: RequestsProxy.Message =
        RequestsProxy.Message.RemoteSlotData(hostId, remoteSlotData.map(_._2))
      (requestsProxy.sendNoWait _).expects(expectedSlotDataMessage).once().returning(().pure[F])

      val localChain = buildLocalChainAlgebra()
      (() => localChain.head).expects().anyNumberOfTimes().returns(bestSlotData.pure[F])
      (localChain.isWorseThan _).expects(bestSlotData).once().returning(true.pure[F])

      val slotDataStoreMap = mutable.Map.empty[BlockId, SlotData]
      slotDataStoreMap.put(knownId, knownSlotData)
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      (slotDataStore.get _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
        slotDataStoreMap.get(id).pure[F]
      }
      (slotDataStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
        slotDataStoreMap.contains(id).pure[F]
      }
      (slotDataStore.put _).expects(*, *).rep(remoteSlotDataCount).onCall { case (id: BlockId, slotData: SlotData) =>
        slotDataStoreMap.put(id, slotData).pure[F].void
      }
      (slotDataStore
        .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId] @unchecked))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
          slotDataStoreMap(id).pure[F]
        }

      val blockIdTree = mock[ParentChildTree[F, BlockId]]
      (blockIdTree.associate _).expects(*, *).rep(remoteSlotDataCount).returning(().pure[F])

      val clock = mock[ClockAlgebra[F]]
      (() => clock.globalSlot).expects().anyNumberOfTimes().returning(2L.pure[F])

      val blockHeights = mock[BlockHeights[F]]

      val commonAncestorF = mock[(BlockchainPeerClient[F], BlockHeights[F], LocalChainAlgebra[F]) => F[BlockId]]
      (commonAncestorF.apply _).expects(*, *, *).returning(knownId.pure[F])

      val bodyStore = mock[Store[F, BlockId, BlockBody]]
      (bodyStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId => (knownId == id).pure[F] }

      PeerBlockHeaderFetcher
        .makeActor(
          hostId,
          client,
          requestsProxy,
          peersManager,
          localChain,
          slotDataStore,
          bodyStore,
          blockIdTree,
          clock,
          blockHeights,
          commonAncestorF
        )
        .use { actor =>
          for {
            state <- actor.send(PeerBlockHeaderFetcher.Message.StartActor)
            _     <- state.fetchingFiber.get.join
          } yield ()
        }
    }
  }

  test("New better slot data and block source shall be sent chunk if local chain is worse") {
    withMock {
      val slotData: NonEmptyChain[SlotData] =
        arbitraryLinkedSlotDataChainFor(Gen.choose(2, maxChainSize)).arbitrary.first
      val idAndSlotData: NonEmptyChain[(BlockId, SlotData)] = slotData.map(s => (s.slotId.blockId, s))
      val (knownId, knownSlotData) = idAndSlotData.head
      val remoteSlotData = NonEmptyChain.fromChain(idAndSlotData.tail).get

      val (bestSlotId, bestSlotData) = remoteSlotData.last
      val remoteIdToSlotData: Map[BlockId, SlotData] = remoteSlotData.toList.toMap
      val remoteSlotDataCount = remoteIdToSlotData.size

      val client = mock[BlockchainPeerClient[F]]
      (client
        .getSlotDataOrError(_: BlockId, _: Throwable)(_: MonadThrow[F]))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, e: BlockHeaderDownloadErrorByName @unchecked, _: MonadThrow[F] @unchecked) =>
          OptionT(remoteIdToSlotData.get(id).pure[F]).getOrRaise(e.apply())
        }
      (() => client.remotePeerAdoptions).expects().once().onCall { () =>
        Stream.eval[F, BlockId](bestSlotId.pure[F]).pure[F]
      }
      val remoteHeightToBlockId = remoteIdToSlotData.map { case (id, sd) => sd.height -> id }
      (client.getRemoteBlockIdAtHeight _).expects(*, *).onCall { case (height: Long, _: Option[BlockId] @unchecked) =>
        remoteHeightToBlockId.get(height).pure[F]
      }

      val peersManager = mock[PeersManagerActor[F]]
      val requestsProxy = mock[RequestsProxyActor[F]]

      val enoughHeightDelta = 1
      val chainSelectionAlgebra: ChainSelectionAlgebra[F, SlotData] = new ChainSelectionAlgebra[F, SlotData] {
        override def compare(x: SlotData, y: SlotData): F[Int] = x.height.compare(y.height).pure[F]

        override def enoughHeightToCompare(currentHeight: Long, commonHeight: Long, proposedHeight: Long): F[Long] =
          (proposedHeight - enoughHeightDelta).pure[F]
      }
      val localChain = mock[LocalChainAlgebra[F]]
      (() => localChain.chainSelectionAlgebra).stubs().returning(chainSelectionAlgebra.pure[F])
      (() => localChain.head).expects().anyNumberOfTimes().returns(bestSlotData.pure[F])
      val req = remoteSlotData.toList.dropRight(enoughHeightDelta)

      (localChain.isWorseThan _).expects(req.last._2).once().returning(true.pure[F])

      val necMes = NonEmptyChain.fromSeq(req).get
      val expectedSourceMessage: PeersManager.Message =
        PeersManager.Message.BlocksSource(necMes.map(d => (hostId, d._1)))
      (peersManager.sendNoWait _).expects(expectedSourceMessage).once().returning(().pure[F])

      val expectedSlotDataMessage: RequestsProxy.Message =
        RequestsProxy.Message.RemoteSlotData(hostId, necMes.map(_._2))
      (requestsProxy.sendNoWait _).expects(expectedSlotDataMessage).once().returning(().pure[F])

      val slotDataStoreMap = mutable.Map.empty[BlockId, SlotData]
      slotDataStoreMap.put(knownId, knownSlotData)
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      (slotDataStore.get _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
        slotDataStoreMap.get(id).pure[F]
      }
      (slotDataStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
        slotDataStoreMap.contains(id).pure[F]
      }
      (slotDataStore.put _).expects(*, *).rep(remoteSlotDataCount - enoughHeightDelta).onCall {
        case (id: BlockId, slotData: SlotData) =>
          slotDataStoreMap.put(id, slotData).pure[F].void
      }
      (slotDataStore
        .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId] @unchecked))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
          slotDataStoreMap(id).pure[F]
        }

      val blockIdTree = mock[ParentChildTree[F, BlockId]]
      (blockIdTree.associate _).expects(*, *).rep(remoteSlotDataCount - enoughHeightDelta).returning(().pure[F])

      val clock = mock[ClockAlgebra[F]]
      (() => clock.globalSlot).expects().anyNumberOfTimes().returning(2L.pure[F])

      val blockHeights = mock[BlockHeights[F]]

      val commonAncestorF = mock[(BlockchainPeerClient[F], BlockHeights[F], LocalChainAlgebra[F]) => F[BlockId]]
      (commonAncestorF.apply _).expects(*, *, *).returning(knownId.pure[F])

      val bodyStore = mock[Store[F, BlockId, BlockBody]]
      (bodyStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId => (knownId == id).pure[F] }

      PeerBlockHeaderFetcher
        .makeActor(
          hostId,
          client,
          requestsProxy,
          peersManager,
          localChain,
          slotDataStore,
          bodyStore,
          blockIdTree,
          clock,
          blockHeights,
          commonAncestorF
        )
        .use { actor =>
          for {
            state <- actor.send(PeerBlockHeaderFetcher.Message.StartActor)
            _     <- state.fetchingFiber.get.join
          } yield ()
        }
    }
  }

  test("New better slot data and block source shall be sent chunk if local chain is worse but we have slot data") {
    withMock {
      val slotData: NonEmptyChain[SlotData] =
        arbitraryLinkedSlotDataChainFor(Gen.choose(2, maxChainSize)).arbitrary.first
      val idAndSlotData: NonEmptyChain[(BlockId, SlotData)] = slotData.map(s => (s.slotId.blockId, s))
      val (knownId, _) = idAndSlotData.head
      val remoteSlotData = NonEmptyChain.fromChain(idAndSlotData.tail).get

      val (bestSlotId, bestSlotData) = remoteSlotData.last
      val remoteIdToSlotData: Map[BlockId, SlotData] = remoteSlotData.toList.toMap

      val client = mock[BlockchainPeerClient[F]]
      (client
        .getSlotDataOrError(_: BlockId, _: Throwable)(_: MonadThrow[F]))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, e: BlockHeaderDownloadErrorByName @unchecked, _: MonadThrow[F] @unchecked) =>
          OptionT(remoteIdToSlotData.get(id).pure[F]).getOrRaise(e.apply())
        }
      (() => client.remotePeerAdoptions).expects().once().onCall { () =>
        Stream.eval[F, BlockId](bestSlotId.pure[F]).pure[F]
      }
      val remoteHeightToBlockId = remoteIdToSlotData.map { case (id, sd) => sd.height -> id }
      (client.getRemoteBlockIdAtHeight _).expects(*, *).onCall { case (height: Long, _: Option[BlockId] @unchecked) =>
        remoteHeightToBlockId.get(height).pure[F]
      }

      val peersManager = mock[PeersManagerActor[F]]
      val requestsProxy = mock[RequestsProxyActor[F]]

      val enoughHeightDelta = 1
      val chainSelectionAlgebra: ChainSelectionAlgebra[F, SlotData] = new ChainSelectionAlgebra[F, SlotData] {
        override def compare(x: SlotData, y: SlotData): F[Int] = x.height.compare(y.height).pure[F]

        override def enoughHeightToCompare(currentHeight: Long, commonHeight: Long, proposedHeight: Long): F[Long] =
          (proposedHeight - enoughHeightDelta).pure[F]
      }
      val localChain = mock[LocalChainAlgebra[F]]
      (() => localChain.chainSelectionAlgebra).stubs().returning(chainSelectionAlgebra.pure[F])
      (() => localChain.head).expects().anyNumberOfTimes().returns(bestSlotData.pure[F])
      val req = remoteSlotData.toList.dropRight(enoughHeightDelta)

      (localChain.isWorseThan _).expects(req.last._2).once().returning(true.pure[F])

      val necMes = NonEmptyChain.fromSeq(req).get
      val expectedSourceMessage: PeersManager.Message =
        PeersManager.Message.BlocksSource(necMes.map(d => (hostId, d._1)))
      (peersManager.sendNoWait _).expects(expectedSourceMessage).once().returning(().pure[F])

      val expectedSlotDataMessage: RequestsProxy.Message =
        RequestsProxy.Message.RemoteSlotData(hostId, necMes.map(_._2))
      (requestsProxy.sendNoWait _).expects(expectedSlotDataMessage).once().returning(().pure[F])

      val slotDataStoreMap = slotData.map(sd => sd.slotId.blockId -> sd).toList.toMap
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      (slotDataStore.get _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
        slotDataStoreMap.get(id).pure[F]
      }
      (slotDataStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
        slotDataStoreMap.contains(id).pure[F]
      }
      (slotDataStore
        .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId] @unchecked))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
          slotDataStoreMap(id).pure[F]
        }

      val blockIdTree = mock[ParentChildTree[F, BlockId]]

      val clock = mock[ClockAlgebra[F]]
      (() => clock.globalSlot).expects().anyNumberOfTimes().returning(2L.pure[F])

      val blockHeights = mock[BlockHeights[F]]

      val commonAncestorF = mock[(BlockchainPeerClient[F], BlockHeights[F], LocalChainAlgebra[F]) => F[BlockId]]
      (commonAncestorF.apply _).expects(*, *, *).returning(knownId.pure[F])

      val bodyStore = mock[Store[F, BlockId, BlockBody]]
      (bodyStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId => (knownId == id).pure[F] }

      PeerBlockHeaderFetcher
        .makeActor(
          hostId,
          client,
          requestsProxy,
          peersManager,
          localChain,
          slotDataStore,
          bodyStore,
          blockIdTree,
          clock,
          blockHeights,
          commonAncestorF
        )
        .use { actor =>
          for {
            state <- actor.send(PeerBlockHeaderFetcher.Message.StartActor)
            _     <- state.fetchingFiber.get.join
          } yield ()
        }
    }
  }

  test("New slot data shall not be sent if local chain is better by density and block source is not interesting") {
    withMock {
      val peersManager = mock[PeersManagerActor[F]]

      val slotData: NonEmptyChain[SlotData] =
        arbitraryLinkedSlotDataChainFor(Gen.choose(2, maxChainSize)).arbitrary.first
      val idAndSlotData: NonEmptyChain[(BlockId, SlotData)] = slotData.map(s => (s.slotId.blockId, s))
      val (knownId, knownSlotData) = idAndSlotData.head
      val remoteSlotData = NonEmptyChain.fromChain(idAndSlotData.tail).get

      val (bestSlotId, bestSlotData) = remoteSlotData.last
      val remoteIdToSlotData: Map[BlockId, SlotData] = remoteSlotData.toList.toMap

      val client = mock[BlockchainPeerClient[F]]
      (client
        .getSlotDataOrError(_: BlockId, _: Throwable)(_: MonadThrow[F]))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, e: BlockHeaderDownloadErrorByName @unchecked, _: MonadThrow[F] @unchecked) =>
          OptionT(remoteIdToSlotData.get(id).pure[F]).getOrRaise(e.apply())
        }
      (() => client.remotePeerAdoptions).expects().once().onCall { () =>
        Stream.eval[F, BlockId](bestSlotId.pure[F]).pure[F]
      }
      val remoteHeightToBlockId = remoteIdToSlotData.map { case (id, sd) => sd.height -> id }
      (client.getRemoteBlockIdAtHeight _).expects(*, *).onCall { case (height: Long, _: Option[BlockId] @unchecked) =>
        remoteHeightToBlockId.get(height).pure[F]
      }

      val requestsProxy = mock[RequestsProxyActor[F]]
      val expectedMessage: RequestsProxy.Message =
        RequestsProxy.Message.RemoteSlotData(hostId, remoteSlotData.map(_._2))
      (requestsProxy.sendNoWait _).expects(expectedMessage).never()
      (requestsProxy.sendNoWait _)
        .expects(RequestsProxy.Message.BadKLookbackSlotData(hostId))
        .once()
        .returning(Applicative[F].unit)

      val localChain = buildLocalChainAlgebra()
      (localChain.isWorseThan _).expects(bestSlotData).once().returning(false.pure[F])
      (() => localChain.head).expects().anyNumberOfTimes().returning(knownSlotData.pure[F])

      val slotDataStoreMap = mutable.Map.empty[BlockId, SlotData]
      slotDataStoreMap.put(knownId, knownSlotData)
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      (slotDataStore.get _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
        slotDataStoreMap.get(id).pure[F]
      }
      (slotDataStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
        slotDataStoreMap.contains(id).pure[F]
      }
      (slotDataStore.put _)
        .expects(*, *)
        .rep(remoteSlotData.size.toInt)
        .onCall { case (id: BlockId, slotData: SlotData) =>
          slotDataStoreMap.put(id, slotData).pure[F].void
        }
      (slotDataStore
        .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId] @unchecked))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
          slotDataStoreMap(id).pure[F]
        }

      val blockIdTree = mock[ParentChildTree[F, BlockId]]
      (blockIdTree.associate _).expects(*, *).anyNumberOfTimes().returning(().pure[F])

      val clock = mock[ClockAlgebra[F]]
      (() => clock.globalSlot).expects().anyNumberOfTimes().returning(2L.pure[F])

      val blockHeights = mock[BlockHeights[F]]

      val commonAncestorF = mock[(BlockchainPeerClient[F], BlockHeights[F], LocalChainAlgebra[F]) => F[BlockId]]
      (commonAncestorF.apply _).expects(*, *, *).returning(knownId.pure[F])

      val bodyStore = mock[Store[F, BlockId, BlockBody]]
      (bodyStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId => (knownId == id).pure[F] }

      PeerBlockHeaderFetcher
        .makeActor(
          hostId,
          client,
          requestsProxy,
          peersManager,
          localChain,
          slotDataStore,
          bodyStore,
          blockIdTree,
          clock,
          blockHeights,
          commonAncestorF
        )
        .use { actor =>
          for {
            state <- actor.send(PeerBlockHeaderFetcher.Message.StartActor)
            _     <- state.fetchingFiber.get.join
          } yield ()
        }
    }
  }

  test("New better slot data and block source shall be NOT be sent if we still process previous slot data") {
    withMock {
      val slotData: NonEmptyChain[SlotData] =
        arbitraryLinkedSlotDataChainFor(Gen.choose(2L, 2L)).arbitrary.first
      val idAndSlotData = slotData.map(s => (s.slotId.blockId, s)).toList

      val (localId, localSlot) = idAndSlotData.head
      val (remote1Id, remote1Slot) = idAndSlotData(1)
      val (remote2Id, _) = idAndSlotData(2)
      val remoteSlotData = idAndSlotData.tail

      val remoteIdToSlotData: Map[BlockId, SlotData] = remoteSlotData.toMap

      val client = mock[BlockchainPeerClient[F]]
      (client
        .getSlotDataOrError(_: BlockId, _: Throwable)(_: MonadThrow[F]))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, e: BlockHeaderDownloadErrorByName @unchecked, _: MonadThrow[F] @unchecked) =>
          OptionT(remoteIdToSlotData.get(id).pure[F]).getOrRaise(e.apply())
        }
      (() => client.remotePeerAdoptions).expects().once().onCall { () =>
        Stream.emits(Seq(remote1Id, remote2Id)).covary.pure[F]
      }

      val peersManager = mock[PeersManagerActor[F]]
      val requestsProxy = mock[RequestsProxyActor[F]]

      (peersManager.sendNoWait _).expects(*).once().returning(().pure[F])
      (requestsProxy.sendNoWait _).expects(*).once().returning(().pure[F])

      val localChain = buildLocalChainAlgebra()
      (() => localChain.head).expects().anyNumberOfTimes().returns(localSlot.pure[F])
      (localChain.isWorseThan _).expects(remote1Slot).once().returning(true.pure[F])

      val slotDataStoreMap = mutable.Map.empty[BlockId, SlotData]
      slotDataStoreMap.put(localId, localSlot)
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      (slotDataStore.get _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
        slotDataStoreMap.get(id).pure[F]
      }
      (slotDataStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
        slotDataStoreMap.contains(id).pure[F]
      }
      (slotDataStore.put _).expects(*, *).once().onCall { case (id: BlockId, slotData: SlotData) =>
        slotDataStoreMap.put(id, slotData).pure[F].void
      }
      (slotDataStore
        .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId] @unchecked))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
          slotDataStoreMap(id).pure[F]
        }

      val blockIdTree = mock[ParentChildTree[F, BlockId]]
      (blockIdTree.associate _).expects(*, *).once().returning(().pure[F])

      val clock = mock[ClockAlgebra[F]]
      (() => clock.globalSlot).expects().anyNumberOfTimes().returning(2L.pure[F])

      val blockHeights = mock[BlockHeights[F]]

      val commonAncestorF = mock[(BlockchainPeerClient[F], BlockHeights[F], LocalChainAlgebra[F]) => F[BlockId]]

      val bodyStore = mock[Store[F, BlockId, BlockBody]]
      (bodyStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId => (localId == id).pure[F] }

      PeerBlockHeaderFetcher
        .makeActor(
          hostId,
          client,
          requestsProxy,
          peersManager,
          localChain,
          slotDataStore,
          bodyStore,
          blockIdTree,
          clock,
          blockHeights,
          commonAncestorF
        )
        .use { actor =>
          for {
            state <- actor.send(PeerBlockHeaderFetcher.Message.StartActor)
            _     <- state.fetchingFiber.get.join
          } yield ()
        }
    }
  }

  test("New better slot data and block source shall be PROCESSED after we process previous slot data") {
    withMock {
      val slotData: NonEmptyChain[SlotData] =
        arbitraryLinkedSlotDataChainFor(Gen.choose(2L, 2L)).arbitrary.first
      val idAndSlotData = slotData.map(s => (s.slotId.blockId, s)).toList

      val (localId, localSlot) = idAndSlotData.head
      val (remote1Id, remote1Slot) = idAndSlotData(1)
      val (remote2Id, remote2Slot) = idAndSlotData(2)
      val remoteSlotData = idAndSlotData.tail

      val remoteIdToSlotData: Map[BlockId, SlotData] = remoteSlotData.toMap

      val client = mock[BlockchainPeerClient[F]]
      (client
        .getSlotDataOrError(_: BlockId, _: Throwable)(_: MonadThrow[F]))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, e: BlockHeaderDownloadErrorByName @unchecked, _: MonadThrow[F] @unchecked) =>
          OptionT(remoteIdToSlotData.get(id).pure[F]).getOrRaise(e.apply())
        }
      (() => client.remotePeerAdoptions).expects().once().onCall { () =>
        Stream.emits(Seq(remote1Id, remote2Id)).covary.pure[F]
      }

      val peersManager = mock[PeersManagerActor[F]]
      val requestsProxy = mock[RequestsProxyActor[F]]

      (peersManager.sendNoWait _).expects(*).twice().returning(().pure[F])
      (requestsProxy.sendNoWait _).expects(*).twice().returning(().pure[F])

      val localChain = buildLocalChainAlgebra()
      (() => localChain.head).expects().once().returns(localSlot.pure[F])
      (() => localChain.head).expects().once().returns(remote1Slot.pure[F])
      (localChain.isWorseThan _).expects(remote1Slot).once().returning(true.pure[F])
      (localChain.isWorseThan _).expects(remote2Slot).once().returning(true.pure[F])

      val slotDataStoreMap = mutable.Map.empty[BlockId, SlotData]
      slotDataStoreMap.put(localId, localSlot)
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      (slotDataStore.get _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
        slotDataStoreMap.get(id).pure[F]
      }
      (slotDataStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
        slotDataStoreMap.contains(id).pure[F]
      }
      (slotDataStore.put _).expects(*, *).twice().onCall { case (id: BlockId, slotData: SlotData) =>
        slotDataStoreMap.put(id, slotData).pure[F].void
      }
      (slotDataStore
        .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId] @unchecked))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
          slotDataStoreMap(id).pure[F]
        }

      val blockIdTree = mock[ParentChildTree[F, BlockId]]
      (blockIdTree.associate _).expects(*, *).twice().returning(().pure[F])

      val clock = mock[ClockAlgebra[F]]
      (() => clock.globalSlot).expects().anyNumberOfTimes().returning(2L.pure[F])

      val blockHeights = mock[BlockHeights[F]]

      val commonAncestorF = mock[(BlockchainPeerClient[F], BlockHeights[F], LocalChainAlgebra[F]) => F[BlockId]]

      val bodyStore = mock[Store[F, BlockId, BlockBody]]
      (bodyStore.contains _).expects(remote1Id).once().returning(false.pure[F])
      (bodyStore.contains _).expects(localId).once().returning(true.pure[F])

      (bodyStore.contains _).expects(remote2Id).once().returning(false.pure[F])
      (bodyStore.contains _).expects(remote1Id).once().returning(true.pure[F])

      PeerBlockHeaderFetcher
        .makeActor(
          hostId,
          client,
          requestsProxy,
          peersManager,
          localChain,
          slotDataStore,
          bodyStore,
          blockIdTree,
          clock,
          blockHeights,
          commonAncestorF
        )
        .use { actor =>
          for {
            state <- actor.send(PeerBlockHeaderFetcher.Message.StartActor)
            _     <- state.fetchingFiber.get.join
          } yield ()
        }
    }
  }

  test("New slot data shall not be sent if we receive known worse block") {
    withMock {
      val peersManager = mock[PeersManagerActor[F]]

      val slotData: NonEmptyChain[SlotData] =
        arbitraryLinkedSlotDataChainFor(Gen.choose(2, maxChainSize)).arbitrary.first
      val idAndSlotData: NonEmptyChain[(BlockId, SlotData)] = slotData.map(s => (s.slotId.blockId, s))
      val (knownId, knownSlotData) = idAndSlotData.last
      val remoteSlotData = NonEmptyChain.fromChain(idAndSlotData.init).get

      val (bestSlotId, _) = remoteSlotData.last
      val remoteIdToSlotData: Map[BlockId, SlotData] = remoteSlotData.toList.toMap

      val client = mock[BlockchainPeerClient[F]]
      (client
        .getSlotDataOrError(_: BlockId, _: Throwable)(_: MonadThrow[F]))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, e: BlockHeaderDownloadErrorByName @unchecked, _: MonadThrow[F] @unchecked) =>
          OptionT(remoteIdToSlotData.get(id).pure[F]).getOrRaise(e.apply())
        }
      (() => client.remotePeerAdoptions).expects().once().onCall { () =>
        Stream.eval[F, BlockId](bestSlotId.pure[F]).pure[F]
      }
      val remoteHeightToBlockId = remoteIdToSlotData.map { case (id, sd) => sd.height -> id }
      (client.getRemoteBlockIdAtHeight _).expects(*, *).onCall { case (height: Long, _: Option[BlockId] @unchecked) =>
        remoteHeightToBlockId.get(height).pure[F]
      }

      val requestsProxy = mock[RequestsProxyActor[F]]
      val expectedMessage: RequestsProxy.Message =
        RequestsProxy.Message.RemoteSlotData(hostId, remoteSlotData.map(_._2))
      (requestsProxy.sendNoWait _).expects(expectedMessage).never()

      val localChain = buildLocalChainAlgebra()
      (() => localChain.head).expects().anyNumberOfTimes().returning(knownSlotData.pure[F])

      val slotDataStoreMap = idAndSlotData.map { case (id, slot) => id -> slot }.toList.toMap

      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      (slotDataStore.get _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
        slotDataStoreMap.get(id).pure[F]
      }
      (slotDataStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
        slotDataStoreMap.contains(id).pure[F]
      }

      (slotDataStore
        .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId] @unchecked))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
          slotDataStoreMap(id).pure[F]
        }

      val blockIdTree = mock[ParentChildTree[F, BlockId]]
      (blockIdTree.associate _).expects(*, *).anyNumberOfTimes().returning(().pure[F])

      val clock = mock[ClockAlgebra[F]]
      (() => clock.globalSlot).expects().anyNumberOfTimes().returning(2L.pure[F])

      val blockHeights = mock[BlockHeights[F]]

      val commonAncestorF = mock[(BlockchainPeerClient[F], BlockHeights[F], LocalChainAlgebra[F]) => F[BlockId]]
      (commonAncestorF.apply _).expects(*, *, *).returning(bestSlotId.pure[F])

      val bodyStore = mock[Store[F, BlockId, BlockBody]]
      (bodyStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId => (knownId == id).pure[F] }

      PeerBlockHeaderFetcher
        .makeActor(
          hostId,
          client,
          requestsProxy,
          peersManager,
          localChain,
          slotDataStore,
          bodyStore,
          blockIdTree,
          clock,
          blockHeights,
          commonAncestorF
        )
        .use { actor =>
          for {
            state <- actor.send(PeerBlockHeaderFetcher.Message.StartActor)
            _     <- state.fetchingFiber.get.join
          } yield ()
        }
    }
  }

  test("New slot data shall not be sent if we receive unknown worse block: build long chain") {
    withMock {
      val peersManager = mock[PeersManagerActor[F]]

      val slotData: NonEmptyChain[SlotData] =
        arbitraryLinkedSlotDataChainFor(Gen.choose(3, maxChainSize)).arbitrary.first
      val idAndSlotData: NonEmptyChain[(BlockId, SlotData)] = slotData.map(s => (s.slotId.blockId, s))
      val (_, knownSlotData) = idAndSlotData.last
      val remoteSlotData =
        arbitraryLinkedSlotDataChainFor(Gen.choose(1L, 1L), idAndSlotData.head._2.some).arbitrary.first
          .map(sd => sd.slotId.blockId -> sd)

      val (bestSlotId, bestSlotData) = remoteSlotData.last
      val remoteIdToSlotData: Map[BlockId, SlotData] = remoteSlotData.toList.toMap

      val client = mock[BlockchainPeerClient[F]]
      (client
        .getSlotDataOrError(_: BlockId, _: Throwable)(_: MonadThrow[F]))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, e: BlockHeaderDownloadErrorByName @unchecked, _: MonadThrow[F] @unchecked) =>
          OptionT(remoteIdToSlotData.get(id).pure[F]).getOrRaise(e.apply())
        }
      (() => client.remotePeerAdoptions).expects().once().onCall { () =>
        Stream.eval[F, BlockId](bestSlotId.pure[F]).pure[F]
      }
      val remoteHeightToBlockId = remoteIdToSlotData.map { case (id, sd) => sd.height -> id }
      (client.getRemoteBlockIdAtHeight _).expects(*, *).onCall { case (height: Long, _: Option[BlockId] @unchecked) =>
        remoteHeightToBlockId.get(height).pure[F]
      }

      val requestsProxy = mock[RequestsProxyActor[F]]
      val expectedMessage: RequestsProxy.Message =
        RequestsProxy.Message.RemoteSlotData(hostId, remoteSlotData.map(_._2))
      (requestsProxy.sendNoWait _).expects(expectedMessage).never()

      val localChain = buildLocalChainAlgebra()
      (() => localChain.head).expects().anyNumberOfTimes().returning(knownSlotData.pure[F])
      (localChain.isWorseThan _).expects(bestSlotData).once().returning(false.pure[F])

      val slotDataStoreMap = mutable.Map() ++ idAndSlotData.map { case (id, slot) => id -> slot }.toList.toMap

      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      (slotDataStore.get _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
        slotDataStoreMap.get(id).pure[F]
      }
      (slotDataStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
        slotDataStoreMap.contains(id).pure[F]
      }
      (slotDataStore.put _)
        .expects(*, *)
        .rep(remoteSlotData.size.toInt)
        .onCall { case (id: BlockId, slotData: SlotData) =>
          slotDataStoreMap.put(id, slotData).pure[F].void
        }

      (slotDataStore
        .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId] @unchecked))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
          slotDataStoreMap(id).pure[F]
        }

      val blockIdTree = mock[ParentChildTree[F, BlockId]]
      (blockIdTree.associate _).expects(*, *).anyNumberOfTimes().returning(().pure[F])

      val clock = mock[ClockAlgebra[F]]
      (() => clock.globalSlot).expects().anyNumberOfTimes().returning(2L.pure[F])

      val blockHeights = mock[BlockHeights[F]]

      val commonAncestorF = mock[(BlockchainPeerClient[F], BlockHeights[F], LocalChainAlgebra[F]) => F[BlockId]]
      (commonAncestorF.apply _).expects(*, *, *).returning(idAndSlotData.head._1.pure[F])

      val bodyStore = mock[Store[F, BlockId, BlockBody]]
      val knownBodies = idAndSlotData.map(_._1).toList.toSet
      (bodyStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
        knownBodies.contains(id).pure[F]
      }

      PeerBlockHeaderFetcher
        .makeActor(
          hostId,
          client,
          requestsProxy,
          peersManager,
          localChain,
          slotDataStore,
          bodyStore,
          blockIdTree,
          clock,
          blockHeights,
          commonAncestorF
        )
        .use { actor =>
          for {
            state <- actor.send(PeerBlockHeaderFetcher.Message.StartActor)
            _     <- state.fetchingFiber.get.join
          } yield ()
        }
    }
  }

  test("New slot data shall not be sent if we receive unknown worse block: one block chain") {
    withMock {
      val peersManager = mock[PeersManagerActor[F]]

      val slotData: NonEmptyChain[SlotData] =
        arbitraryLinkedSlotDataChainFor(Gen.choose(3, maxChainSize)).arbitrary.first
      val idAndSlotData: NonEmptyChain[(BlockId, SlotData)] = slotData.map(s => (s.slotId.blockId, s))
      val (_, knownSlotData) = idAndSlotData.last
      val remoteSlotData =
        arbitraryLinkedSlotDataChainFor(Gen.choose(0L, 0L), idAndSlotData.head._2.some).arbitrary.first
          .map(sd => sd.slotId.blockId -> sd)

      val (bestSlotId, bestSlotData) = remoteSlotData.last
      val remoteIdToSlotData: Map[BlockId, SlotData] = remoteSlotData.toList.toMap

      val client = mock[BlockchainPeerClient[F]]
      (client
        .getSlotDataOrError(_: BlockId, _: Throwable)(_: MonadThrow[F]))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, e: BlockHeaderDownloadErrorByName @unchecked, _: MonadThrow[F] @unchecked) =>
          OptionT(remoteIdToSlotData.get(id).pure[F]).getOrRaise(e.apply())
        }
      (() => client.remotePeerAdoptions).expects().once().onCall { () =>
        Stream.eval[F, BlockId](bestSlotId.pure[F]).pure[F]
      }

      val requestsProxy = mock[RequestsProxyActor[F]]
      val expectedMessage: RequestsProxy.Message =
        RequestsProxy.Message.RemoteSlotData(hostId, remoteSlotData.map(_._2))
      (requestsProxy.sendNoWait _).expects(expectedMessage).never()

      val localChain = buildLocalChainAlgebra()
      (() => localChain.head).expects().anyNumberOfTimes().returning(knownSlotData.pure[F])
      (localChain.isWorseThan _).expects(bestSlotData).once().returning(false.pure[F])

      val slotDataStoreMap = mutable.Map() ++ idAndSlotData.map { case (id, slot) => id -> slot }.toList.toMap

      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      (slotDataStore.get _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
        slotDataStoreMap.get(id).pure[F]
      }
      (slotDataStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
        slotDataStoreMap.contains(id).pure[F]
      }
      (slotDataStore.put _)
        .expects(*, *)
        .rep(remoteSlotData.size.toInt)
        .onCall { case (id: BlockId, slotData: SlotData) =>
          slotDataStoreMap.put(id, slotData).pure[F].void
        }

      (slotDataStore
        .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId] @unchecked))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
          slotDataStoreMap(id).pure[F]
        }

      val blockIdTree = mock[ParentChildTree[F, BlockId]]
      (blockIdTree.associate _).expects(*, *).anyNumberOfTimes().returning(().pure[F])

      val clock = mock[ClockAlgebra[F]]
      (() => clock.globalSlot).expects().anyNumberOfTimes().returning(2L.pure[F])

      val blockHeights = mock[BlockHeights[F]]

      val commonAncestorF = mock[(BlockchainPeerClient[F], BlockHeights[F], LocalChainAlgebra[F]) => F[BlockId]]

      val bodyStore = mock[Store[F, BlockId, BlockBody]]
      val knownBodies = idAndSlotData.map(_._1).toList.toSet
      (bodyStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
        knownBodies.contains(id).pure[F]
      }

      PeerBlockHeaderFetcher
        .makeActor(
          hostId,
          client,
          requestsProxy,
          peersManager,
          localChain,
          slotDataStore,
          bodyStore,
          blockIdTree,
          clock,
          blockHeights,
          commonAncestorF
        )
        .use { actor =>
          for {
            state <- actor.send(PeerBlockHeaderFetcher.Message.StartActor)
            _     <- state.fetchingFiber.get.join
          } yield ()
        }
    }
  }

  test("Sent only block source if we receive current head") {
    withMock {
      val slotData = arbitrarySlotData.arbitrary.first

      val client = mock[BlockchainPeerClient[F]]

      (() => client.remotePeerAdoptions).expects().once().onCall { () =>
        Stream.eval[F, BlockId](slotData.slotId.blockId.pure[F]).pure[F]
      }

      val peersManager = mock[PeersManagerActor[F]]
      (peersManager.sendNoWait _)
        .expects(PeersManager.Message.BlocksSource(NonEmptyChain.one(hostId -> slotData.slotId.blockId)))
        .returning(Applicative[F].unit)

      val requestsProxy = mock[RequestsProxyActor[F]]

      val localChain = buildLocalChainAlgebra()
      (() => localChain.head).expects().anyNumberOfTimes().returning(slotData.pure[F])

      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      (slotDataStore.get _).expects(slotData.slotId.blockId).anyNumberOfTimes().returning(slotData.some.pure[F])
      (slotDataStore.contains _).expects(slotData.parentSlotId.blockId).anyNumberOfTimes().returning(true.pure[F])
      (slotDataStore.contains _).expects(slotData.slotId.blockId).anyNumberOfTimes().returning(true.pure[F])
      (slotDataStore
        .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
        .expects(slotData.slotId.blockId, *, *)
        .anyNumberOfTimes()
        .returning(slotData.pure[F])

      val blockIdTree = mock[ParentChildTree[F, BlockId]]

      val clock = mock[ClockAlgebra[F]]
      (() => clock.globalSlot).expects().anyNumberOfTimes().returning(2L.pure[F])

      val blockHeights = mock[BlockHeights[F]]

      val commonAncestorF =
        mock[(BlockchainPeerClient[F], BlockHeights[F], LocalChainAlgebra[F]) => F[BlockId]]

      val bodyStore = mock[Store[F, BlockId, BlockBody]]
      (bodyStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
        (slotData.slotId.blockId == id).pure[F]
      }

      PeerBlockHeaderFetcher
        .makeActor(
          hostId,
          client,
          requestsProxy,
          peersManager,
          localChain,
          slotDataStore,
          bodyStore,
          blockIdTree,
          clock,
          blockHeights,
          commonAncestorF
        )
        .use { actor =>
          for {
            state <- actor.send(PeerBlockHeaderFetcher.Message.StartActor)
            _     <- state.fetchingFiber.get.join
          } yield ()
        }

    }
  }

  test("Sent only block source if we receive worse block with same height") {
    withMock {
      val commonAncestor = arbitrarySlotData.arbitrary.first.copy(height = 5)
      val thisHead = arbitrarySlotData.arbitrary.first.copy(parentSlotId = commonAncestor.slotId, height = 6)
      val remoteKnownHead = arbitrarySlotData.arbitrary.first.copy(parentSlotId = commonAncestor.slotId, height = 6)

      val client = mock[BlockchainPeerClient[F]]

      (() => client.remotePeerAdoptions).expects().once().onCall { () =>
        Stream.eval[F, BlockId](remoteKnownHead.slotId.blockId.pure[F]).pure[F]
      }

      val peersManager = mock[PeersManagerActor[F]]
      (peersManager.sendNoWait _)
        .expects(PeersManager.Message.BlocksSource(NonEmptyChain.one(hostId -> remoteKnownHead.slotId.blockId)))
        .returning(Applicative[F].unit)

      val requestsProxy = mock[RequestsProxyActor[F]]

      val localChain = buildLocalChainAlgebra()
      (() => localChain.head).expects().anyNumberOfTimes().returning(thisHead.pure[F])

      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      (slotDataStore.get _)
        .expects(remoteKnownHead.slotId.blockId)
        .anyNumberOfTimes()
        .returning(remoteKnownHead.some.pure[F])
      (slotDataStore.contains _)
        .expects(remoteKnownHead.parentSlotId.blockId)
        .anyNumberOfTimes()
        .returning(true.pure[F])
      (slotDataStore.contains _).expects(remoteKnownHead.slotId.blockId).anyNumberOfTimes().returning(true.pure[F])
      (slotDataStore
        .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
        .expects(remoteKnownHead.slotId.blockId, *, *)
        .anyNumberOfTimes()
        .returning(remoteKnownHead.pure[F])

      val blockIdTree = mock[ParentChildTree[F, BlockId]]

      val clock = mock[ClockAlgebra[F]]
      (() => clock.globalSlot).expects().anyNumberOfTimes().returning(2L.pure[F])

      val blockHeights = mock[BlockHeights[F]]

      val commonAncestorF = mock[(BlockchainPeerClient[F], BlockHeights[F], LocalChainAlgebra[F]) => F[BlockId]]

      val bodyStore = mock[Store[F, BlockId, BlockBody]]
      (bodyStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
        (remoteKnownHead.slotId.blockId == id).pure[F]
      }

      PeerBlockHeaderFetcher
        .makeActor(
          hostId,
          client,
          requestsProxy,
          peersManager,
          localChain,
          slotDataStore,
          bodyStore,
          blockIdTree,
          clock,
          blockHeights,
          commonAncestorF
        )
        .use { actor =>
          for {
            state <- actor.send(PeerBlockHeaderFetcher.Message.StartActor)
            _     <- state.fetchingFiber.get.join
          } yield ()
        }

    }
  }

  test("Requested tip shall be sent if local chain is worse") {
    withMock {
      val slotData: NonEmptyChain[SlotData] =
        arbitraryLinkedSlotDataChainFor(Gen.choose(2, maxChainSize)).arbitrary.first
      val idAndSlotData: NonEmptyChain[(BlockId, SlotData)] = slotData.map(s => (s.slotId.blockId, s))
      val (knownId, knownSlotData) = idAndSlotData.head
      val remoteSlotData = NonEmptyChain.fromChain(idAndSlotData.tail).get

      val remoteIdToSlotData: Map[BlockId, SlotData] = remoteSlotData.toList.toMap

      val bestTip = idAndSlotData.last._2

      val client = mock[BlockchainPeerClient[F]]
      (client.remoteCurrentTip _)
        .expects()
        .returns(Option(bestTip.slotId.blockId).pure[F])
      (() => client.remotePeerAdoptions).expects().once().onCall { () =>
        Stream.fromOption[F](Option.empty[BlockId]).pure[F]
      }
      (client
        .getSlotDataOrError(_: BlockId, _: Throwable)(_: MonadThrow[F]))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, e: BlockHeaderDownloadErrorByName @unchecked, _: MonadThrow[F] @unchecked) =>
          OptionT(remoteIdToSlotData.get(id).pure[F]).getOrRaise(e.apply())
        }
      val remoteHeightToBlockId = remoteIdToSlotData.map { case (id, sd) => sd.height -> id }
      (client.getRemoteBlockIdAtHeight _).expects(*, *).onCall { case (height: Long, _: Option[BlockId] @unchecked) =>
        remoteHeightToBlockId.get(height).pure[F]
      }

      val peersManager = mock[PeersManagerActor[F]]
      val requestsProxy = mock[RequestsProxyActor[F]]

      val expectedSourceMessage: PeersManager.Message =
        PeersManager.Message.BlocksSource(remoteSlotData.map(d => (hostId, d._1)))
      (peersManager.sendNoWait _).expects(expectedSourceMessage).once().returning(().pure[F])
      val expectedSlotDataMessage: RequestsProxy.Message =
        RequestsProxy.Message.RemoteSlotData(hostId, remoteSlotData.map(_._2))
      (requestsProxy.sendNoWait _).expects(expectedSlotDataMessage).once().returning(().pure[F])

      val localChain = buildLocalChainAlgebra()
      (localChain.isWorseThan _).expects(bestTip).once().returning(true.pure[F])
      (() => localChain.head).expects().anyNumberOfTimes().returning(knownSlotData.pure[F])

      val slotDataStoreMap = mutable.Map.empty[BlockId, SlotData]
      slotDataStoreMap.put(knownId, knownSlotData)
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      (slotDataStore.get _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
        slotDataStoreMap.get(id).pure[F]
      }
      (slotDataStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
        slotDataStoreMap.contains(id).pure[F]
      }
      (slotDataStore.put _).expects(*, *).anyNumberOfTimes().onCall { case (id: BlockId, slotData: SlotData) =>
        slotDataStoreMap.put(id, slotData).pure[F].void
      }
      (slotDataStore
        .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId] @unchecked))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
          slotDataStoreMap(id).pure[F]
        }

      val blockIdTree = mock[ParentChildTree[F, BlockId]]
      (blockIdTree.associate _).expects(*, *).anyNumberOfTimes().returning(().pure[F])

      val clock = mock[ClockAlgebra[F]]
      (() => clock.globalSlot).expects().anyNumberOfTimes().returning(2L.pure[F])

      val blockHeights = mock[BlockHeights[F]]

      val commonAncestorF = mock[(BlockchainPeerClient[F], BlockHeights[F], LocalChainAlgebra[F]) => F[BlockId]]
      (commonAncestorF.apply _).expects(*, *, *).returning(knownId.pure[F])

      val bodyStore = mock[Store[F, BlockId, BlockBody]]
      (bodyStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId => (knownId == id).pure[F] }

      PeerBlockHeaderFetcher
        .makeActor(
          hostId,
          client,
          requestsProxy,
          peersManager,
          localChain,
          slotDataStore,
          bodyStore,
          blockIdTree,
          clock,
          blockHeights,
          commonAncestorF
        )
        .use { actor =>
          for {
            _     <- actor.send(PeerBlockHeaderFetcher.Message.StartActor)
            state <- actor.send(PeerBlockHeaderFetcher.Message.GetCurrentTip)
            _     <- state.fetchingFiber.get.join
          } yield ()
        }
    }
  }

  test("Bad KLookback shall be send because tip could be better after full check") {
    withMock {
      val peersManager = mock[PeersManagerActor[F]]

      val slotData: NonEmptyChain[SlotData] =
        arbitraryLinkedSlotDataChainFor(Gen.choose(2, maxChainSize)).arbitrary.first
      val idAndSlotData: NonEmptyChain[(BlockId, SlotData)] = slotData.map(s => (s.slotId.blockId, s))
      val (knownId, knownSlotData) = idAndSlotData.head
      val remoteSlotData = NonEmptyChain.fromChain(idAndSlotData.tail).get

      val remoteIdToSlotData: Map[BlockId, SlotData] = remoteSlotData.toList.toMap

      val bestTip = idAndSlotData.last._2

      val client = mock[BlockchainPeerClient[F]]
      (client.remoteCurrentTip _)
        .expects()
        .returns(Option(bestTip.slotId.blockId).pure[F])
      (() => client.remotePeerAdoptions).expects().once().onCall { () =>
        Stream.fromOption[F](Option.empty[BlockId]).pure[F]
      }
      (client
        .getSlotDataOrError(_: BlockId, _: Throwable)(_: MonadThrow[F]))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, e: BlockHeaderDownloadErrorByName @unchecked, _: MonadThrow[F] @unchecked) =>
          OptionT(remoteIdToSlotData.get(id).pure[F]).getOrRaise(e.apply())
        }
      val remoteHeightToBlockId = remoteIdToSlotData.map { case (id, sd) => sd.height -> id }
      (client.getRemoteBlockIdAtHeight _).expects(*, *).onCall { case (height: Long, _: Option[BlockId] @unchecked) =>
        remoteHeightToBlockId.get(height).pure[F]
      }

      val requestsProxy = mock[RequestsProxyActor[F]]
      val expectedBadKLookbackMessage: RequestsProxy.Message = RequestsProxy.Message.BadKLookbackSlotData(hostId)
      (requestsProxy.sendNoWait _).expects(expectedBadKLookbackMessage).once().returning(().pure[F])

      val localChain = buildLocalChainAlgebra()
      (localChain.isWorseThan _).expects(bestTip).once().returning(false.pure[F])
      val head = arbitrarySlotData.arbitrary.first.copy(height = 1)
      (() => localChain.head).expects().anyNumberOfTimes().returning(head.pure[F])

      val slotDataStoreMap = mutable.Map.empty[BlockId, SlotData]
      slotDataStoreMap.put(knownId, knownSlotData)
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      (slotDataStore.get _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
        slotDataStoreMap.get(id).pure[F]
      }
      (slotDataStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
        slotDataStoreMap.contains(id).pure[F]
      }
      (slotDataStore.put _).expects(*, *).anyNumberOfTimes().onCall { case (id: BlockId, slotData: SlotData) =>
        slotDataStoreMap.put(id, slotData).pure[F].void
      }
      (slotDataStore
        .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId] @unchecked))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
          slotDataStoreMap(id).pure[F]
        }

      val blockIdTree = mock[ParentChildTree[F, BlockId]]
      (blockIdTree.associate _).expects(*, *).anyNumberOfTimes().returning(().pure[F])

      val clock = mock[ClockAlgebra[F]]
      (() => clock.globalSlot).expects().anyNumberOfTimes().returning(2L.pure[F])

      val blockHeights = mock[BlockHeights[F]]

      val commonAncestorF = mock[(BlockchainPeerClient[F], BlockHeights[F], LocalChainAlgebra[F]) => F[BlockId]]
      (commonAncestorF.apply _).expects(*, *, *).returning(knownId.pure[F])

      val bodyStore = mock[Store[F, BlockId, BlockBody]]
      (bodyStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId => (knownId == id).pure[F] }

      PeerBlockHeaderFetcher
        .makeActor(
          hostId,
          client,
          requestsProxy,
          peersManager,
          localChain,
          slotDataStore,
          bodyStore,
          blockIdTree,
          clock,
          blockHeights,
          commonAncestorF
        )
        .use { actor =>
          for {
            _     <- actor.send(PeerBlockHeaderFetcher.Message.StartActor)
            state <- actor.send(PeerBlockHeaderFetcher.Message.GetCurrentTip)
            _     <- state.fetchingFiber.get.join
          } yield ()
        }
    }
  }

  test("Requested tip shall not be sent if local chain is better") {
    withMock {
      val peersManager = mock[PeersManagerActor[F]]

      val slotData: NonEmptyChain[SlotData] =
        arbitraryLinkedSlotDataChainFor(Gen.choose(2, maxChainSize)).arbitrary.first
      val idAndSlotData: NonEmptyChain[(BlockId, SlotData)] = slotData.map(s => (s.slotId.blockId, s))
      val (knownId, knownSlotData) = idAndSlotData.head
      val remoteSlotData = NonEmptyChain.fromChain(idAndSlotData.tail).get

      val remoteIdToSlotData: Map[BlockId, SlotData] = remoteSlotData.toList.toMap

      val bestTip = idAndSlotData.last._2

      val client = mock[BlockchainPeerClient[F]]
      (client.remoteCurrentTip _)
        .expects()
        .returns(Option(bestTip.slotId.blockId).pure[F])
      (() => client.remotePeerAdoptions).expects().once().onCall { () =>
        Stream.fromOption[F](Option.empty[BlockId]).pure[F]
      }
      (client
        .getSlotDataOrError(_: BlockId, _: Throwable)(_: MonadThrow[F]))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, e: BlockHeaderDownloadErrorByName @unchecked, _: MonadThrow[F] @unchecked) =>
          OptionT(remoteIdToSlotData.get(id).pure[F]).getOrRaise(e.apply())
        }
      val remoteHeightToBlockId = remoteIdToSlotData.map { case (id, sd) => sd.height -> id }
      (client.getRemoteBlockIdAtHeight _).expects(*, *).onCall { case (height: Long, _: Option[BlockId] @unchecked) =>
        remoteHeightToBlockId.get(height).pure[F]
      }

      val requestsProxy = mock[RequestsProxyActor[F]]
      val expectedMessage: RequestsProxy.Message = RequestsProxy.Message.BadKLookbackSlotData(hostId)
      (requestsProxy.sendNoWait _).expects(expectedMessage).once().returns(().pure[F])

      val localChain = buildLocalChainAlgebra()
      (() => localChain.head).expects().anyNumberOfTimes().returning(knownSlotData.pure[F])
      (localChain.isWorseThan _).expects(bestTip).once().returning(false.pure[F])

      val slotDataStoreMap = mutable.Map.empty[BlockId, SlotData]
      slotDataStoreMap.put(knownId, knownSlotData)
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      (slotDataStore.get _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
        slotDataStoreMap.get(id).pure[F]
      }
      (slotDataStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
        slotDataStoreMap.contains(id).pure[F]
      }
      (slotDataStore.put _).expects(*, *).anyNumberOfTimes().onCall { case (id: BlockId, slotData: SlotData) =>
        slotDataStoreMap.put(id, slotData).pure[F].void
      }
      (slotDataStore
        .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId] @unchecked))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
          slotDataStoreMap(id).pure[F]
        }

      val blockIdTree = mock[ParentChildTree[F, BlockId]]
      (blockIdTree.associate _).expects(*, *).anyNumberOfTimes().returning(().pure[F])

      val clock = mock[ClockAlgebra[F]]
      (() => clock.globalSlot).expects().anyNumberOfTimes().returning(2L.pure[F])

      val blockHeights = mock[BlockHeights[F]]

      val commonAncestorF = mock[(BlockchainPeerClient[F], BlockHeights[F], LocalChainAlgebra[F]) => F[BlockId]]
      (commonAncestorF.apply _).expects(*, *, *).returning(knownId.pure[F])

      val bodyStore = mock[Store[F, BlockId, BlockBody]]
      (bodyStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId => (knownId == id).pure[F] }

      PeerBlockHeaderFetcher
        .makeActor(
          hostId,
          client,
          requestsProxy,
          peersManager,
          localChain,
          slotDataStore,
          bodyStore,
          blockIdTree,
          clock,
          blockHeights,
          commonAncestorF
        )
        .use { actor =>
          for {
            _     <- actor.send(PeerBlockHeaderFetcher.Message.StartActor)
            state <- actor.send(PeerBlockHeaderFetcher.Message.GetCurrentTip)
            _     <- state.fetchingFiber.get.join
          } yield ()
        }

    }
  }

  test("New slot data should be rejected if the genesis timestamp has not elapsed") {
    withMock {
      val slotData = arbitrarySlotData.arbitrary.first.update(_.slotId.slot.set(5L))

      val peersManager = mock[PeersManagerActor[F]]
      (peersManager.sendNoWait _)
        .expects(PeersManager.Message.NonCriticalErrorForHost(hostId))
        .returns(Applicative[F].unit)

      val client = mock[BlockchainPeerClient[F]]
      (() => client.remotePeerAdoptions)
        .expects()
        .once()
        .returning(Stream(slotData.slotId.blockId).covaryAll[F, BlockId].pure[F])
      (client
        .getSlotDataOrError[Throwable](_: BlockId, _: Throwable)(_: MonadThrow[F]))
        .expects(slotData.slotId.blockId, *, *)
        .once()
        .returning(slotData.pure[F])

      // The requestsProxy should never be called in this situation
      val requestsProxy = mock[RequestsProxyActor[F]]

      val localChain = buildLocalChainAlgebra()

      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      (slotDataStore
        .get(_: BlockId))
        .expects(slotData.slotId.blockId)
        .once()
        .returning(none[SlotData].pure[F])

      val blockIdTree = mock[ParentChildTree[F, BlockId]]

      val clock = mock[ClockAlgebra[F]]
      (() => clock.globalSlot)
        .expects()
        .once()
        .returning((-1L).pure[F])

      val blockHeights = mock[BlockHeights[F]]

      val commonAncestorF = mock[(BlockchainPeerClient[F], BlockHeights[F], LocalChainAlgebra[F]) => F[BlockId]]

      val bodyStore = mock[Store[F, BlockId, BlockBody]]
      (bodyStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
        (slotData.slotId.blockId == id).pure[F]
      }

      PeerBlockHeaderFetcher
        .makeActor(
          hostId,
          client,
          requestsProxy,
          peersManager,
          localChain,
          slotDataStore,
          bodyStore,
          blockIdTree,
          clock,
          blockHeights,
          commonAncestorF
        )
        .use { actor =>
          for {
            _ <- Sync[F].andWait(
              actor.sendNoWait(PeerBlockHeaderFetcher.Message.StartActor),
              FiniteDuration(100, MILLISECONDS)
            )
          } yield ()
        }
    }
  }
}
