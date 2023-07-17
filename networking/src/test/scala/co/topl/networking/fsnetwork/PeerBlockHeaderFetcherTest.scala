package co.topl.networking.fsnetwork

import cats.MonadThrow
import cats.data.{NonEmptyChain, OptionT}
import cats.effect.{Async, IO}
import cats.implicits._
import co.topl.algebras.Store
import co.topl.codecs.bytes.tetra.instances._
import co.topl.consensus.algebras.LocalChainAlgebra
import co.topl.consensus.models.{BlockHeader, BlockId, SlotData}
import co.topl.eventtree.ParentChildTree
import co.topl.models.ModelGenerators.GenHelper
import co.topl.models.generators.consensus.ModelGenerators._
import co.topl.networking.blockchain.BlockchainPeerClient
import co.topl.networking.fsnetwork.BlockDownloadError.BlockHeaderDownloadError
import co.topl.networking.fsnetwork.BlockDownloadError.BlockHeaderDownloadError._
import co.topl.networking.fsnetwork.PeerBlockHeaderFetcherTest.{BlockHeaderDownloadErrorByName, F}
import co.topl.networking.fsnetwork.RequestsProxy.RequestsProxyActor
import co.topl.networking.fsnetwork.TestHelper._
import co.topl.typeclasses.implicits._
import fs2.Stream
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.Gen
import org.scalamock.function.FunctionAdapter1
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

  val hostId: HostId = "127.0.0.1"
  val maxChainSize = 99

  private def compareWithoutDownloadTimeMatcher(
    rawExpectedMessage: RequestsProxy.Message
  ): FunctionAdapter1[RequestsProxy.Message, Boolean] = {
    val matchingFunction: RequestsProxy.Message => Boolean =
      (rawActualMessage: RequestsProxy.Message) =>
        (rawExpectedMessage, rawActualMessage) match {
          case (
                expectedMessage: RequestsProxy.Message.DownloadHeadersResponse,
                actualMessage: RequestsProxy.Message.DownloadHeadersResponse
              ) =>
            val newResp =
              actualMessage.response.map { case (header, res) =>
                (header, res.map(b => b.copy(downloadTimeMs = 0)))
              }
            expectedMessage == actualMessage.copy(response = newResp)
        }
    new FunctionAdapter1[RequestsProxy.Message, Boolean](matchingFunction)
  }

  test("Block header shall be downloaded by request") {
    withMock {
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
        .expects(compareWithoutDownloadTimeMatcher(expectedMessage))
        .once()
        .returning(().pure[F])

      val localChain = mock[LocalChainAlgebra[F]]

      val slotDataStore = mock[Store[F, BlockId, SlotData]]

      val blockIdTree = mock[ParentChildTree[F, BlockId]]

      PeerBlockHeaderFetcher
        .makeActor(hostId, client, requestsProxy, localChain, slotDataStore, blockIdTree)
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

      val header: BlockHeader = arbitraryHeader.arbitrary.first.embedId
      val client = mock[BlockchainPeerClient[F]]
      (client
        .getHeaderOrError(_: BlockId, _: Throwable)(_: MonadThrow[F]))
        .expects(header.id, *, *)
        .once
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

      val localChain = mock[LocalChainAlgebra[F]]

      val slotDataStore = mock[Store[F, BlockId, SlotData]]

      val blockIdTree = mock[ParentChildTree[F, BlockId]]

      PeerBlockHeaderFetcher
        .makeActor(hostId, client, requestsProxy, localChain, slotDataStore, blockIdTree)
        .use { actor =>
          for {
            _ <- actor.sendNoWait(PeerBlockHeaderFetcher.Message.DownloadBlockHeaders(NonEmptyChain.one(header.id)))
          } yield ()
        }
    }
  }

  test("Block header shall be downloaded by request, for missing headers error shall be returned") {
    withMock {
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
        .expects(compareWithoutDownloadTimeMatcher(expectedMessage))
        .once()
        .returning(().pure[F])

      val localChain = mock[LocalChainAlgebra[F]]

      val slotDataStore = mock[Store[F, BlockId, SlotData]]

      val blockIdTree = mock[ParentChildTree[F, BlockId]]

      PeerBlockHeaderFetcher
        .makeActor(hostId, client, requestsProxy, localChain, slotDataStore, blockIdTree)
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
        .expects(compareWithoutDownloadTimeMatcher(expectedMessage))
        .once()
        .returning(().pure[F])

      val localChain = mock[LocalChainAlgebra[F]]

      val slotDataStore = mock[Store[F, BlockId, SlotData]]

      val blockIdTree = mock[ParentChildTree[F, BlockId]]

      PeerBlockHeaderFetcher
        .makeActor(hostId, client, requestsProxy, localChain, slotDataStore, blockIdTree)
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

      val requestsProxy = mock[RequestsProxyActor[F]]
      val expectedSourceMessage: RequestsProxy.Message =
        RequestsProxy.Message.BlocksSource(remoteSlotData.map(d => (hostId, d._1)))
      (requestsProxy.sendNoWait _).expects(expectedSourceMessage).once().returning(().pure[F])
      val expectedSlotDataMessage: RequestsProxy.Message =
        RequestsProxy.Message.RemoteSlotData(hostId, remoteSlotData.map(_._2))
      (requestsProxy.sendNoWait _).expects(expectedSlotDataMessage).once().returning(().pure[F])

      val localChain = mock[LocalChainAlgebra[F]]
      (localChain.couldBeWorse _).expects(bestSlotData).once().returns(true.pure[F])
      (localChain.isWorseThan _).expects(bestSlotData).once().returning(true.pure[F])

      val slotDataStoreMap = mutable.Map.empty[BlockId, SlotData]
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      (slotDataStore.get _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
        { if (id === knownId) Option(knownSlotData) else None }.pure[F]
      }
      (slotDataStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
        (!remoteIdToSlotData.contains(id)).pure[F]
      }
      (slotDataStore.put _).expects(*, *).rep(remoteSlotDataCount).onCall { case (id: BlockId, slotData: SlotData) =>
        slotDataStoreMap.put(id, slotData).pure[F].void
      }

      val blockIdTree = mock[ParentChildTree[F, BlockId]]
      (blockIdTree.associate _).expects(*, *).rep(remoteSlotDataCount).returning(().pure[F])

      PeerBlockHeaderFetcher
        .makeActor(hostId, client, requestsProxy, localChain, slotDataStore, blockIdTree)
        .use { actor =>
          for {
            state <- actor.send(PeerBlockHeaderFetcher.Message.StartActor)
            _     <- state.fetchingFiber.get.join
          } yield ()
        }
    }
  }

  test("New better slot data shall not be sent if local chain is better and block source is not interesting") {
    withMock {
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

      val requestsProxy = mock[RequestsProxyActor[F]]
      val expectedMessage: RequestsProxy.Message =
        RequestsProxy.Message.RemoteSlotData(hostId, remoteSlotData.map(_._2))
      (requestsProxy.sendNoWait _).expects(expectedMessage).never()

      val localChain = mock[LocalChainAlgebra[F]]
      (localChain.couldBeWorse _).expects(bestSlotData).once().returning(false.pure[F])
      (localChain.head _).expects().once().returning(knownSlotData.pure[F])

      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      (slotDataStore.get _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
        {
          if (id === knownId) Option(knownSlotData) else None
        }.pure[F]
      }
      (slotDataStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
        (!remoteIdToSlotData.contains(id)).pure[F]
      }

      val blockIdTree = mock[ParentChildTree[F, BlockId]]

      PeerBlockHeaderFetcher
        .makeActor(hostId, client, requestsProxy, localChain, slotDataStore, blockIdTree)
        .use { actor =>
          for {
            state <- actor.send(PeerBlockHeaderFetcher.Message.StartActor)
            _     <- state.fetchingFiber.get.join
          } yield ()
        }

    }
  }

  test("Sent only block source if local chain is better and block source is interesting") {
    withMock {
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

      val requestsProxy = mock[RequestsProxyActor[F]]
      val expectedMessage: RequestsProxy.Message =
        RequestsProxy.Message.BlocksSource(NonEmptyChain.one((hostId, bestSlotData.slotId.blockId)))
      (requestsProxy.sendNoWait _).expects(expectedMessage).never()

      val localChain = mock[LocalChainAlgebra[F]]
      (localChain.couldBeWorse _).expects(bestSlotData).once().returning(false.pure[F])
      (localChain.head _).expects().once().returning(knownSlotData.pure[F])

      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      (slotDataStore.get _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
        {
          if (id === knownId) Option(knownSlotData) else None
        }.pure[F]
      }
      (slotDataStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
        (!remoteIdToSlotData.contains(id)).pure[F]
      }

      val blockIdTree = mock[ParentChildTree[F, BlockId]]

      PeerBlockHeaderFetcher
        .makeActor(hostId, client, requestsProxy, localChain, slotDataStore, blockIdTree)
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

      val requestsProxy = mock[RequestsProxyActor[F]]
      val expectedSourceMessage: RequestsProxy.Message =
        RequestsProxy.Message.BlocksSource(remoteSlotData.map(d => (hostId, d._1)))
      (requestsProxy.sendNoWait _).expects(expectedSourceMessage).once().returning(().pure[F])
      val expectedSlotDataMessage: RequestsProxy.Message =
        RequestsProxy.Message.RemoteSlotData(hostId, remoteSlotData.map(_._2))
      (requestsProxy.sendNoWait _).expects(expectedSlotDataMessage).once().returning(().pure[F])

      val localChain = mock[LocalChainAlgebra[F]]
      (localChain.couldBeWorse _).expects(bestTip).once().returns(true.pure[F])
      (localChain.isWorseThan _).expects(bestTip).once().returning(true.pure[F])

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

      val blockIdTree = mock[ParentChildTree[F, BlockId]]
      (blockIdTree.associate _).expects(*, *).anyNumberOfTimes().returning(().pure[F])

      PeerBlockHeaderFetcher
        .makeActor(hostId, client, requestsProxy, localChain, slotDataStore, blockIdTree)
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

      val requestsProxy = mock[RequestsProxyActor[F]]
      val expectedMessage: RequestsProxy.Message =
        RequestsProxy.Message.RemoteSlotData(hostId, remoteSlotData.map(_._2))
      (requestsProxy.sendNoWait _).expects(expectedMessage).never()

      val localChain = mock[LocalChainAlgebra[F]]
      (localChain.couldBeWorse _).expects(bestTip).once().returning(false.pure[F])
      (localChain.head _).expects().once().returning(knownSlotData.pure[F])

      val slotDataStoreMap = mutable.Map.empty[BlockId, SlotData]
      slotDataStoreMap.put(knownId, knownSlotData)
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      (slotDataStore.get _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
        slotDataStoreMap.get(id).pure[F]
      }
      (slotDataStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
        slotDataStoreMap.contains(id).pure[F]
      }

      val blockIdTree = mock[ParentChildTree[F, BlockId]]

      PeerBlockHeaderFetcher
        .makeActor(hostId, client, requestsProxy, localChain, slotDataStore, blockIdTree)
        .use { actor =>
          for {
            _     <- actor.send(PeerBlockHeaderFetcher.Message.StartActor)
            state <- actor.send(PeerBlockHeaderFetcher.Message.GetCurrentTip)
            _     <- state.fetchingFiber.get.join
          } yield ()
        }

    }
  }
}
