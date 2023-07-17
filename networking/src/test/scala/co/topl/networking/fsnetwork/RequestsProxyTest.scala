package co.topl.networking.fsnetwork

import cats.data.NonEmptyChain
import cats.effect.IO
import cats.implicits._
import co.topl.algebras.Store
import co.topl.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import co.topl.consensus.models.{BlockHeader, BlockId, SlotData}
import co.topl.models.ModelGenerators.GenHelper
import co.topl.models.generators.consensus.ModelGenerators.{arbitraryBlockId, arbitraryHeader, nonEmptyChainArbOf}
import co.topl.models.generators.node.ModelGenerators
import co.topl.networking.fsnetwork.BlockChecker.BlockCheckerActor
import co.topl.networking.fsnetwork.BlockDownloadError.BlockBodyDownloadError.BodyNotFoundInPeer
import co.topl.networking.fsnetwork.BlockDownloadError.BlockHeaderDownloadError.HeaderNotFoundInPeer
import co.topl.networking.fsnetwork.BlockDownloadError.{BlockBodyDownloadError, BlockHeaderDownloadError}
import co.topl.networking.fsnetwork.PeersManager.PeersManagerActor
import co.topl.networking.fsnetwork.ReputationAggregator.ReputationAggregatorActor
import co.topl.networking.fsnetwork.RequestsProxyTest.RequestStatus.{InProgress, NewRequest, ReceivedOk}
import co.topl.networking.fsnetwork.RequestsProxyTest.ResponseStatus.{DownloadError, DownloadedOk}
import co.topl.networking.fsnetwork.RequestsProxyTest.{F, RequestStatus, ResponseStatus}
import co.topl.node.models.{Block, BlockBody}
import com.github.benmanes.caffeine.cache.{Cache, Caffeine}
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.Gen
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import co.topl.models.generators.consensus.ModelGenerators._
import co.topl.networking.fsnetwork.TestHelper.arbitraryHostBlockId

import scala.jdk.CollectionConverters._
import scala.collection.immutable.ListMap

object RequestsProxyTest {
  type F[A] = IO[A]

  sealed abstract class RequestStatus

  object RequestStatus {
    object InProgress extends RequestStatus
    object ReceivedOk extends RequestStatus
    object NewRequest extends RequestStatus

    def getFor(o: Any): RequestStatus =
      Math.abs(o.hashCode()) % 3 match {
        case 0 => InProgress
        case 1 => ReceivedOk
        case 2 => NewRequest
      }
  }

  sealed abstract class ResponseStatus

  object ResponseStatus {
    object DownloadedOk extends ResponseStatus
    object DownloadError extends ResponseStatus

    def getFor(o: Any): ResponseStatus =
      Math.abs(o.hashCode()) % 2 match {
        case 0 => DownloadedOk
        case 1 => DownloadError
      }
  }
}

class RequestsProxyTest extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory with ModelGenerators {
  implicit val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)

  val hostId: HostId = "127.0.0.1"
  val maxChainSize = 99

  test("Block header download request: downloaded prefix sent back, request for new block header shall be sent") {
    PropF.forAllF(nonEmptyChainArbOf(arbitraryHeader).arbitrary.retryUntil(_.size < maxChainSize)) {
      headers: NonEmptyChain[BlockHeader] =>
        withMock {

          val reputationAggregator = mock[ReputationAggregatorActor[F]]

          val peersManager = mock[PeersManagerActor[F]]
          val headerStore = mock[Store[F, BlockId, BlockHeader]]
          val bodyStore = mock[Store[F, BlockId, BlockBody]]
          val blockChecker = mock[BlockCheckerActor[F]]
          val headerWithStatus = headers.map(h => (h, RequestStatus.getFor(h)))

          val headerRequestsCache: Cache[BlockId, Option[UnverifiedBlockHeader]] =
            Caffeine.newBuilder.maximumSize(requestCacheSize).build[BlockId, Option[UnverifiedBlockHeader]]()
          headerWithStatus
            .collect {
              case (h, InProgress) => (h.id, None)
              case (h, ReceivedOk) => (h.id, Option(UnverifiedBlockHeader(hostId, h, 0)))
            }
            .map { case (id, headerOpt) => headerRequestsCache.put(id, headerOpt) }

          NonEmptyChain.fromChain(headerWithStatus.collect { case (h, NewRequest) => h.id }).map { req =>
            (peersManager.sendNoWait _)
              .expects(PeersManager.Message.BlockHeadersRequest(hostId, req))
              .returns(().pure[F])
          }

          def inHeaderStorage(id: BlockId): Boolean = id.hashCode() % 2 == 0

          val headerStorageData =
            headerWithStatus.collect { case (h, ReceivedOk) => h.id }.filter(inHeaderStorage).toList.toSet

          (headerStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
            headerStorageData.contains(id).pure[F]
          }

          val blockCheckerMessageOpt =
            for {
              alreadyDownloadedHeaders <-
                NonEmptyChain.fromSeq(
                  headerWithStatus.takeWhile_ { case (_, status) => status == ReceivedOk }.map(_._1)
                )
              newHeaders <-
                NonEmptyChain.fromSeq(alreadyDownloadedHeaders.dropWhile_(h => headerStorageData.contains(h.id)))
            } yield newHeaders

          blockCheckerMessageOpt.map { m =>
            val unverifiedHeaders = m.map(UnverifiedBlockHeader(hostId, _, 0))
            (blockChecker.sendNoWait _)
              .expects(BlockChecker.Message.RemoteBlockHeaders(unverifiedHeaders))
              .returns(().pure[F])
          }

          RequestsProxy
            .makeActor(reputationAggregator, peersManager, headerStore, bodyStore, headerRequests = headerRequestsCache)
            .use { actor =>
              for {
                _     <- actor.send(RequestsProxy.Message.SetupBlockChecker(blockChecker))
                state <- actor.send(RequestsProxy.Message.DownloadHeadersRequest(hostId, headers.map(_.id)))
                _ = assert(headers.map(h => h.id).forall(state.headerRequests.contains))
              } yield ()
            }
        }
    }
  }

  test("Block header download response: downloaded prefix sent back, cache is updated") {
    PropF.forAllF(nonEmptyChainArbOf(arbitraryHeader).arbitrary.retryUntil(_.size < maxChainSize)) {
      headers: NonEmptyChain[BlockHeader] =>
        withMock {

          val reputationAggregator = mock[ReputationAggregatorActor[F]]
          val peersManager = mock[PeersManagerActor[F]]
          val headerStore = mock[Store[F, BlockId, BlockHeader]]
          val bodyStore = mock[Store[F, BlockId, BlockBody]]
          val blockChecker = mock[BlockCheckerActor[F]]

          val headerWithStatus =
            headers.map(h => (h, ResponseStatus.getFor(h)))

          val response = headerWithStatus.map {
            case (header, DownloadedOk)  => (header.id, Either.right(UnverifiedBlockHeader(hostId, header, 0)))
            case (header, DownloadError) => (header.id, Either.left(HeaderNotFoundInPeer: BlockHeaderDownloadError))
          }

          def inHeaderStorage(id: BlockId): Boolean = id.hashCode() % 2 == 0
          (headerStore.get _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
            if (inHeaderStorage(id)) Option(arbitraryHeader.arbitrary.first).pure[F]
            else Option.empty[BlockHeader].pure[F]
          }
          (headerStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
            inHeaderStorage(id).pure[F]
          }

          response
            .collect { case (id, Right(header)) => (id, header) }
            .dropWhile_ { case (id, _) => inHeaderStorage(id) }
            .headOption
            .map {
              case (_, header) if inHeaderStorage(header.blockHeader.parentHeaderId) =>
                (blockChecker.sendNoWait _)
                  .expects(
                    BlockChecker.Message.RemoteBlockHeaders(NonEmptyChain.one(header))
                  )
                  .returns(().pure[F])
              case _ =>
            }

          response.collect { case (_, Right(_)) =>
            (reputationAggregator.sendNoWait _)
              .expects(ReputationAggregator.Message.DownloadTimeHeader(hostId, 0))
              .returns(().pure[F])
          }

          response.collect { case (id, Left(_)) =>
            (reputationAggregator.sendNoWait _)
              .expects(ReputationAggregator.Message.HostProvideIncorrectBlock(hostId))
              .returns(().pure[F])
            (peersManager.sendNoWait _)
              .expects(PeersManager.Message.BlockHeadersRequest(hostId, NonEmptyChain.one(id)))
              .returns(().pure[F])
          }

          RequestsProxy
            .makeActor(reputationAggregator, peersManager, headerStore, bodyStore)
            .use { actor =>
              for {
                _     <- actor.send(RequestsProxy.Message.SetupBlockChecker(blockChecker))
                state <- actor.send(RequestsProxy.Message.DownloadHeadersResponse(hostId, response))
                _ = assert(
                  headerWithStatus
                    .collect { case (header, DownloadedOk) => header.id }
                    .forall(state.headerRequests.contains)
                )
              } yield ()
            }
        }
    }
  }

  test("Block body download request: downloaded prefix sent back, request for new block body shall be sent") {
    PropF.forAllF(TestHelper.arbitraryLinkedSlotDataHeaderBlockNoTx(Gen.choose(1, maxChainSize)).arbitrary) {
      data: NonEmptyChain[(BlockId, SlotData, BlockHeader, BlockBody)] =>
        withMock {
          val dataMap: ListMap[BlockId, (BlockHeader, BlockBody)] =
            ListMap.from(data.toList.map(d => (d._1, (d._3, d._4))))

          val reputationAggregator = mock[ReputationAggregatorActor[F]]
          val peersManager = mock[PeersManagerActor[F]]
          val headerStore = mock[Store[F, BlockId, BlockHeader]]
          val bodyStore = mock[Store[F, BlockId, BlockBody]]
          val blockChecker = mock[BlockCheckerActor[F]]

          val ids = dataMap.keys.toSeq
          val idsWithStatus = ids.map(id => (id, RequestStatus.getFor(id)))

          val bodyRequestsCache: Cache[BlockId, Option[UnverifiedBlockBody]] =
            Caffeine.newBuilder.maximumSize(requestCacheSize).build[BlockId, Option[UnverifiedBlockBody]]()
          idsWithStatus
            .collect {
              case (id, InProgress) => (id, None)
              case (id, ReceivedOk) => (id, Option(UnverifiedBlockBody(hostId, dataMap(id)._2, 0)))
            }
            .foreach { case (id, blockBodyOpt) => bodyRequestsCache.put(id, blockBodyOpt) }

          NonEmptyChain.fromSeq(idsWithStatus.collect { case (id, NewRequest) => id }).map { blockIds =>
            (peersManager.sendNoWait _)
              .expects(PeersManager.Message.BlockBodyRequest(hostId, blockIds.map(id => dataMap(id)._1)))
              .returns(().pure[F])
          }

          def inBodyStorage(id: BlockId): Boolean = id.hashCode() % 2 == 0
          val bodyStorageData =
            idsWithStatus.collect { case (id, ReceivedOk) => id }.filter(inBodyStorage).toList.toSet
          (bodyStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
            bodyStorageData.contains(id).pure[F]
          }

          val headerStorageData = dataMap.view.mapValues { case (header, _) => header }.toMap
          (headerStore.get _).expects(*).anyNumberOfTimes().onCall { id: BlockId => headerStorageData.get(id).pure[F] }

          val blockCheckerMessageOpt =
            for {
              alreadyDownloadedBodies <-
                NonEmptyChain.fromSeq(idsWithStatus.takeWhile_ { case (_, status) => status == ReceivedOk }.map(_._1))
              newBodies <-
                NonEmptyChain.fromSeq(alreadyDownloadedBodies.dropWhile_(id => bodyStorageData.contains(id)))
            } yield newBodies.map { id =>
              val d = dataMap(id)
              Block(d._1, d._2)
            }

          blockCheckerMessageOpt.map { m =>
            val data = m.map(d => (d.header, UnverifiedBlockBody(hostId, d.body, 0)))
            (blockChecker.sendNoWait _).expects(BlockChecker.Message.RemoteBlockBodies(data)).returns(().pure[F])
          }

          val message = RequestsProxy.Message.DownloadBodiesRequest(
            hostId,
            NonEmptyChain.fromSeq(ids.map(id => dataMap(id)._1)).get
          )

          RequestsProxy
            .makeActor(reputationAggregator, peersManager, headerStore, bodyStore, bodyRequests = bodyRequestsCache)
            .use { actor =>
              for {
                _     <- actor.send(RequestsProxy.Message.SetupBlockChecker(blockChecker))
                state <- actor.send(message)
                _ = assert(ids.forall(state.bodyRequests.contains))
              } yield ()
            }
        }
    }
  }

  test("Block body download response: downloaded prefix sent back, cache is updated") {
    PropF.forAllF(TestHelper.arbitraryLinkedSlotDataHeaderBlockNoTx(Gen.choose(1, maxChainSize)).arbitrary) {
      data: NonEmptyChain[(BlockId, SlotData, BlockHeader, BlockBody)] =>
        withMock {
          val dataMap: ListMap[BlockId, (BlockHeader, BlockBody)] =
            ListMap.from(data.toList.map(d => (d._1, (d._3, d._4))))
          val parentOfFirstBlock: BlockId = dataMap.head._2._1.parentHeaderId

          val reputationAggregator = mock[ReputationAggregatorActor[F]]
          val peersManager = mock[PeersManagerActor[F]]
          val headerStore = mock[Store[F, BlockId, BlockHeader]]
          val bodyStore = mock[Store[F, BlockId, BlockBody]]
          val blockChecker = mock[BlockCheckerActor[F]]

          val headersWithStatus =
            dataMap.keys.map(id => (dataMap(id)._1, ResponseStatus.getFor(id.value))).toSeq

          val response = headersWithStatus.map {
            case (header, DownloadedOk)  => (header, Either.right(dataMap(header.id)._2))
            case (header, DownloadError) => (header, Either.left(BodyNotFoundInPeer: BlockBodyDownloadError))
          }

          def inBodyStorage(id: BlockId): Boolean = id.hashCode() % 2 == 0

          // body of parent block for given request
          (bodyStore.get _)
            .expects(parentOfFirstBlock)
            .anyNumberOfTimes()
            .returns(
              if (inBodyStorage(parentOfFirstBlock)) Option(arbitraryNodeBody.arbitrary.first).pure[F] else None.pure[F]
            )

          (bodyStore.get _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
            if (inBodyStorage(id)) dataMap.get(id).map(_._2).pure[F]
            else Option.empty[BlockBody].pure[F]
          }
          (bodyStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
            inBodyStorage(id).pure[F]
          }
          (bodyStore.contains _).expects(parentOfFirstBlock).anyNumberOfTimes().onCall { id: BlockId =>
            inBodyStorage(id).pure[F]
          }
          (headerStore.get _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
            dataMap.get(id).map(_._1).pure[F]
          }

          response
            .collect { case (header, Right(body)) => (header, body) }
            .dropWhile_ { case (header, _) => inBodyStorage(header.id) }
            .headOption
            .map {
              case (header, body) if inBodyStorage(header.parentHeaderId) =>
                (blockChecker.sendNoWait _)
                  .expects(
                    BlockChecker.Message
                      .RemoteBlockBodies(NonEmptyChain.one((header, UnverifiedBlockBody(hostId, body, 0))))
                  )
                  .returns(().pure[F])
              case _ =>
            }

          response
            .collect { case (_, Right(_)) =>
              (reputationAggregator.sendNoWait _)
                .expects(ReputationAggregator.Message.DownloadTimeBody(hostId, 0, Seq.empty))
                .returns(().pure[F])
            }

          response.collect { case (header, Left(_)) =>
            (reputationAggregator.sendNoWait _)
              .expects(ReputationAggregator.Message.HostProvideIncorrectBlock(hostId))
              .returns(().pure[F])
            (peersManager.sendNoWait _)
              .expects(PeersManager.Message.BlockBodyRequest(hostId, NonEmptyChain.one(header)))
              .returns(().pure[F])
          }

          val messageToSend = response.map { case (header, errorOrBody) =>
            (header, errorOrBody.map(UnverifiedBlockBody(hostId, _, 0)))
          }
          RequestsProxy
            .makeActor(reputationAggregator, peersManager, headerStore, bodyStore)
            .use { actor =>
              for {
                _ <- actor.send(RequestsProxy.Message.SetupBlockChecker(blockChecker))
                state <- actor.send(
                  RequestsProxy.Message.DownloadBodiesResponse(hostId, NonEmptyChain.fromSeq(messageToSend).get)
                )
                _ = assert(
                  headersWithStatus
                    .collect { case (header, DownloadedOk) => header.id }
                    .forall(state.bodyRequests.contains)
                )
              } yield ()
            }
        }
    }
  }

  test("Get all tips shall be forwarded to peers manager") {
    withMock {
      val reputationAggregator = mock[ReputationAggregatorActor[F]]
      val peersManager = mock[PeersManagerActor[F]]
      val headerStore = mock[Store[F, BlockId, BlockHeader]]
      val bodyStore = mock[Store[F, BlockId, BlockBody]]

      (peersManager.sendNoWait _)
        .expects(PeersManager.Message.GetCurrentTips)
        .returns(().pure[F])

      RequestsProxy
        .makeActor(reputationAggregator, peersManager, headerStore, bodyStore)
        .use { actor =>
          for {
            _ <- actor.send(RequestsProxy.Message.GetCurrentTips)
          } yield ()
        }

    }
  }

  test("Block invalidation shall leads to banning host") {
    withMock {
      val reputationAggregator = mock[ReputationAggregatorActor[F]]
      val peersManager = mock[PeersManagerActor[F]]
      val headerStore = mock[Store[F, BlockId, BlockHeader]]
      val bodyStore = mock[Store[F, BlockId, BlockBody]]

      (reputationAggregator.sendNoWait _)
        .expects(ReputationAggregator.Message.HostProvideIncorrectBlock(hostId))
        .returns(().pure[F])

      RequestsProxy
        .makeActor(reputationAggregator, peersManager, headerStore, bodyStore)
        .use { actor =>
          for {
            _ <- actor.send(RequestsProxy.Message.InvalidateBlockId(hostId, arbitraryBlockId.arbitrary.first))
          } yield ()
        }

    }
  }

  test("BadKLoopbackSlotData shall be resent to reputation aggregator") {
    withMock {
      val reputationAggregator = mock[ReputationAggregatorActor[F]]
      val peersManager = mock[PeersManagerActor[F]]
      val headerStore = mock[Store[F, BlockId, BlockHeader]]
      val bodyStore = mock[Store[F, BlockId, BlockBody]]

      (reputationAggregator.sendNoWait _)
        .expects(ReputationAggregator.Message.BadKLookbackSlotData(hostId))
        .returns(().pure[F])

      RequestsProxy
        .makeActor(reputationAggregator, peersManager, headerStore, bodyStore)
        .use { actor =>
          for {
            _ <- actor.send(RequestsProxy.Message.BadKLoopbackSlotData(hostId))
          } yield ()
        }

    }
  }

  test("Block source shall be sent to reputation aggregator with correct count") {
    PropF.forAllF(nonEmptyChainArbOfLen(arbitraryHostBlockId, maxChainSize).arbitrary) {
      inputData: NonEmptyChain[(HostId, BlockId)] =>
        withMock {
          val reputationAggregator = mock[ReputationAggregatorActor[F]]
          val peersManager = mock[PeersManagerActor[F]]
          val headerStore = mock[Store[F, BlockId, BlockHeader]]
          val bodyStore = mock[Store[F, BlockId, BlockBody]]

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
            .expects(ReputationAggregator.Message.HostsNoveltyProviding(expectedData))
            .once()
            .returns(().pure[F])

          RequestsProxy
            .makeActor(reputationAggregator, peersManager, headerStore, bodyStore, blockSource = initialCash)
            .use { actor =>
              for {
                _ <- actor.send(RequestsProxy.Message.BlocksSource(inputData))
                // if we send the same data no message is sent
                _ <- actor.send(RequestsProxy.Message.BlocksSource(inputData))
              } yield ()
            }
        }
    }
  }
}
