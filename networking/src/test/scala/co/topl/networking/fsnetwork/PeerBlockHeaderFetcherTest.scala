package co.topl.networking.fsnetwork

import cats.MonadThrow
import cats.data.{NonEmptyChain, OptionT}
import cats.effect.IO
import cats.implicits._
import co.topl.algebras.Store
import co.topl.codecs.bytes.tetra.instances._
import co.topl.consensus.algebras.LocalChainAlgebra
import co.topl.consensus.models.{BlockHeader, BlockId, SlotData}
import co.topl.eventtree.ParentChildTree
import co.topl.models.ModelGenerators.GenHelper
import co.topl.models.generators.consensus.ModelGenerators._
import co.topl.networking.blockchain.BlockchainPeerClient
import co.topl.networking.fsnetwork.BlockChecker.BlockCheckerActor
import co.topl.networking.fsnetwork.BlockDownloadError.BlockHeaderDownloadError
import co.topl.networking.fsnetwork.BlockDownloadError.BlockHeaderDownloadError._
import co.topl.networking.fsnetwork.PeerBlockHeaderFetcherTest.{BlockHeaderDownloadErrorByName, F}
import co.topl.networking.fsnetwork.RequestsProxy.RequestsProxyActor
import co.topl.networking.fsnetwork.TestHelper._
import co.topl.typeclasses.implicits._
import fs2.Stream
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.collection.mutable

object PeerBlockHeaderFetcherTest {
  type F[A] = IO[A]
  type BlockHeaderDownloadErrorByName = () => BlockHeaderDownloadError
}

class PeerBlockHeaderFetcherTest extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {
  implicit val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)

  val hostId: HostId = "127.0.0.1"
  val maxChainSize = 99

  test("Block header shall be downloaded by request") {
    withMock {
      val headers: NonEmptyChain[BlockHeader] =
        nonEmptyChainArbOf(arbitraryHeader).arbitrary.retryUntil(_.size < maxChainSize).first
      val idAndHeader: NonEmptyChain[(BlockId, BlockHeader)] =
        headers.map(h => (h.id, h))
      val idToHeader: Map[BlockId, BlockHeader] = idAndHeader.toList.toMap
      val blocksCount = idToHeader.size

      val client = mock[BlockchainPeerClient[F]]
      (client
        .getHeaderOrError(_: BlockId, _: Throwable)(_: MonadThrow[F]))
        .expects(*, *, *)
        .rep(blocksCount)
        .onCall { case (id: BlockId, e: BlockHeaderDownloadErrorByName, _: MonadThrow[F]) =>
          OptionT(idToHeader.get(id).pure[F]).getOrRaise(e.apply())
        }

      val blockChecker = mock[BlockCheckerActor[F]]

      val requestsProxy = mock[RequestsProxyActor[F]]
      val expectedMessage: RequestsProxy.Message =
        RequestsProxy.Message.DownloadHeadersResponse(
          hostId,
          idAndHeader.map { case (id, header) => (id, Either.right(header)) }
        )
      (requestsProxy.sendNoWait _).expects(expectedMessage).once().returning(().pure[F])

      val localChain = mock[LocalChainAlgebra[F]]

      val slotDataStore = mock[Store[F, BlockId, SlotData]]

      val blockIdTree = mock[ParentChildTree[F, BlockId]]

      PeerBlockHeaderFetcher
        .makeActor(hostId, client, blockChecker, requestsProxy, localChain, slotDataStore, blockIdTree)
        .use { actor =>
          for {
            _ <- actor.sendNoWait(PeerBlockHeaderFetcher.Message.DownloadBlockHeaders(idAndHeader.map(_._1)))
          } yield ()
        }
    }
  }

  test("Block header shall be downloaded by request, for missing headers error shall be returned") {
    withMock {
      val headers: NonEmptyChain[BlockHeader] =
        nonEmptyChainArbOf(arbitraryHeader).arbitrary.retryUntil(_.size < maxChainSize).first
      val idAndHeader: NonEmptyChain[(BlockId, BlockHeader)] =
        headers.map(h => (h.id, h))

      def missedBlockId(id: BlockId): Boolean = (id.hashCode() % 2) == 0

      val idToHeaderOnClient: Map[BlockId, BlockHeader] = idAndHeader.toList.filterNot(d => missedBlockId(d._1)).toMap
      val client = mock[BlockchainPeerClient[F]]
      (client
        .getHeaderOrError(_: BlockId, _: Throwable)(_: MonadThrow[F]))
        .expects(*, *, *)
        .rep(idAndHeader.size.toInt)
        .onCall { case (id: BlockId, e: BlockHeaderDownloadErrorByName, _: MonadThrow[F]) =>
          OptionT(idToHeaderOnClient.get(id).pure[F]).getOrRaise(e.apply())
        }

      val blockChecker = mock[BlockCheckerActor[F]]

      val requestsProxy = mock[RequestsProxyActor[F]]
      val expectedMessage: RequestsProxy.Message =
        RequestsProxy.Message.DownloadHeadersResponse(
          hostId,
          idAndHeader.map { case (id, header) =>
            if (missedBlockId(id)) {
              (id, Either.left(HeaderNotFoundInPeer))
            } else {
              (id, Either.right(header))
            }
          }
        )
      (requestsProxy.sendNoWait _).expects(expectedMessage).once().returning(().pure[F])

      val localChain = mock[LocalChainAlgebra[F]]

      val slotDataStore = mock[Store[F, BlockId, SlotData]]

      val blockIdTree = mock[ParentChildTree[F, BlockId]]

      PeerBlockHeaderFetcher
        .makeActor(hostId, client, blockChecker, requestsProxy, localChain, slotDataStore, blockIdTree)
        .use { actor =>
          for {
            _ <- actor.sendNoWait(PeerBlockHeaderFetcher.Message.DownloadBlockHeaders(idAndHeader.map(_._1)))
          } yield ()
        }
    }
  }

  test("Block header shall be downloaded by request, incorrect headers shall be skipped") {
    withMock {
      val headers: NonEmptyChain[BlockHeader] =
        nonEmptyChainArbOf(arbitraryHeader).arbitrary.retryUntil(_.size < maxChainSize).first
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
        .onCall { case (id: BlockId, e: BlockHeaderDownloadErrorByName, _: MonadThrow[F]) =>
          if (missedBlockId(id)) {
            incorrectHeader.pure[F]
          } else {
            OptionT(idToHeaderOnClient.get(id).pure[F]).getOrRaise(e.apply())
          }
        }

      val blockChecker = mock[BlockCheckerActor[F]]

      val requestsProxy = mock[RequestsProxyActor[F]]
      val expectedMessage: RequestsProxy.Message =
        RequestsProxy.Message.DownloadHeadersResponse(
          hostId,
          idAndHeader.map { case (id, header) =>
            if (missedBlockId(id)) {
              (id, Either.left(HeaderHaveIncorrectId(id, incorrectHeaderId)))
            } else {
              (id, Either.right(header))
            }
          }
        )
      (requestsProxy.sendNoWait _).expects(expectedMessage).once().returning(().pure[F])

      val localChain = mock[LocalChainAlgebra[F]]

      val slotDataStore = mock[Store[F, BlockId, SlotData]]

      val blockIdTree = mock[ParentChildTree[F, BlockId]]

      PeerBlockHeaderFetcher
        .makeActor(hostId, client, blockChecker, requestsProxy, localChain, slotDataStore, blockIdTree)
        .use { actor =>
          for {
            _ <- actor.sendNoWait(PeerBlockHeaderFetcher.Message.DownloadBlockHeaders(idAndHeader.map(_._1)))
          } yield ()
        }
    }
  }

  test("New better slot data shall be sent if local chain is worse") {
    withMock {
      val slotData: NonEmptyChain[SlotData] =
        arbitraryLinkedSlotDataChain.arbitrary.retryUntil(c => c.size > 1 && c.size < maxChainSize).first
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
        .rep(remoteSlotDataCount)
        .onCall { case (id: BlockId, e: BlockHeaderDownloadErrorByName, _: MonadThrow[F]) =>
          OptionT(remoteIdToSlotData.get(id).pure[F]).getOrRaise(e.apply())
        }
      (client.remotePeerAdoptions _).expects().once().onCall { () =>
        Stream.eval[F, BlockId](bestSlotId.pure[F]).pure[F]
      }

      val blockChecker = mock[BlockCheckerActor[F]]
      val expectedMessage: BlockChecker.Message = BlockChecker.Message.RemoteSlotData(hostId, remoteSlotData.map(_._2))
      (blockChecker.sendNoWait _).expects(expectedMessage).once().returning(().pure[F])

      val requestsProxy = mock[RequestsProxyActor[F]]

      val localChain = mock[LocalChainAlgebra[F]]
      (localChain.isWorseThan _).expects(bestSlotData).once().returning(true.pure[F])

      val slotDataStoreMap = mutable.Map.empty[BlockId, SlotData]
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      (slotDataStore.get _).expects(*).rep(remoteSlotDataCount + 1).onCall { id: BlockId =>
        { if (id === knownId) Option(knownSlotData) else None }.pure[F]
      }
      (slotDataStore.contains _).expects(*).rep(remoteSlotDataCount + 2).onCall { id: BlockId =>
        (!remoteIdToSlotData.contains(id)).pure[F]
      }
      (slotDataStore.put _).expects(*, *).rep(remoteSlotDataCount).onCall { case (id: BlockId, slotData: SlotData) =>
        slotDataStoreMap.put(id, slotData).pure[F].void
      }

      val blockIdTree = mock[ParentChildTree[F, BlockId]]
      (blockIdTree.associate _).expects(*, *).rep(remoteSlotDataCount).returning(().pure[F])

      PeerBlockHeaderFetcher
        .makeActor(hostId, client, blockChecker, requestsProxy, localChain, slotDataStore, blockIdTree)
        .use { actor =>
          for {
            state <- actor.send(PeerBlockHeaderFetcher.Message.StartActor)
            _     <- actor.send(PeerBlockHeaderFetcher.Message.StopActor)
            _     <- state.fetchingFiber.get.join
          } yield ()
        }
    }
  }

  test("New better slot data shall not be sent if local chain is better") {
    withMock {
      val slotData: NonEmptyChain[SlotData] =
        arbitraryLinkedSlotDataChain.arbitrary.retryUntil(c => c.size > 1 && c.size < maxChainSize).first
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
        .rep(remoteSlotDataCount)
        .onCall { case (id: BlockId, e: BlockHeaderDownloadErrorByName, _: MonadThrow[F]) =>
          OptionT(remoteIdToSlotData.get(id).pure[F]).getOrRaise(e.apply())
        }
      (client.remotePeerAdoptions _).expects().once().onCall { () =>
        Stream.eval[F, BlockId](bestSlotId.pure[F]).pure[F]
      }

      val blockChecker = mock[BlockCheckerActor[F]]
      val expectedMessage: BlockChecker.Message = BlockChecker.Message.RemoteSlotData(hostId, remoteSlotData.map(_._2))
      (blockChecker.sendNoWait _).expects(expectedMessage).never()

      val requestsProxy = mock[RequestsProxyActor[F]]

      val localChain = mock[LocalChainAlgebra[F]]
      (localChain.isWorseThan _).expects(bestSlotData).once().returning(false.pure[F])

      val slotDataStoreMap = mutable.Map.empty[BlockId, SlotData]
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      (slotDataStore.get _).expects(*).rep(remoteSlotDataCount + 1).onCall { id: BlockId =>
        {
          if (id === knownId) Option(knownSlotData) else None
        }.pure[F]
      }
      (slotDataStore.contains _).expects(*).rep(remoteSlotDataCount + 2).onCall { id: BlockId =>
        (!remoteIdToSlotData.contains(id)).pure[F]
      }
      (slotDataStore.put _).expects(*, *).rep(remoteSlotDataCount).onCall { case (id: BlockId, slotData: SlotData) =>
        slotDataStoreMap.put(id, slotData).pure[F].void
      }

      val blockIdTree = mock[ParentChildTree[F, BlockId]]
      (blockIdTree.associate _).expects(*, *).rep(remoteSlotDataCount).returning(().pure[F])

      PeerBlockHeaderFetcher
        .makeActor(hostId, client, blockChecker, requestsProxy, localChain, slotDataStore, blockIdTree)
        .use { actor =>
          for {
            state <- actor.send(PeerBlockHeaderFetcher.Message.StartActor)
            _     <- actor.send(PeerBlockHeaderFetcher.Message.StopActor)
            _     <- state.fetchingFiber.get.join
          } yield ()
        }

    }
  }
}
