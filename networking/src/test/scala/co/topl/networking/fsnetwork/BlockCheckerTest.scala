package co.topl.networking.fsnetwork

import cats.data._
import cats.effect.IO
import cats.implicits._
import cats.MonadThrow
import cats.Show
import co.topl.algebras.Store
import co.topl.brambl.models.Datum
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.codecs.bytes.tetra.instances._
import co.topl.consensus.algebras._
import co.topl.consensus.models.BlockHeaderValidationFailures.NonForwardSlot
import co.topl.consensus.models._
import co.topl.ledger.algebras._
import co.topl.ledger.models.BodySemanticErrors.TransactionSemanticErrors
import co.topl.ledger.models.TransactionSemanticErrors.InputDataMismatch
import co.topl.ledger.models._
import co.topl.models.ModelGenerators.GenHelper
import co.topl.models.generators.consensus.ModelGenerators._
import co.topl.models.generators.node.ModelGenerators
import co.topl.networking.fsnetwork.BlockCheckerTest.F
import co.topl.networking.fsnetwork.PeersManager.PeersManagerActor
import co.topl.networking.fsnetwork.ReputationAggregator.ReputationAggregatorActor
import co.topl.networking.fsnetwork.RequestsProxy.RequestsProxyActor
import co.topl.networking.fsnetwork.TestHelper._
import co.topl.node.models.{Block, BlockBody}
import co.topl.quivr.runtime.DynamicContext
import co.topl.typeclasses.implicits._
import munit.CatsEffectSuite
import munit.ScalaCheckEffectSuite
import org.scalacheck.Gen
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.collection.mutable

object BlockCheckerTest {
  type F[A] = IO[A]
}

class BlockCheckerTest extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {
  implicit val logger: Logger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)
  val hostId: HostId = "127.0.0.1"
  val maxChainSize = 99

  test("RemoteSlotData: Request no headers if new slot data is worse") {
    PropF.forAllF(arbitraryLinkedSlotDataChainFor(Gen.choose(1, maxChainSize)).arbitrary) {
      slotData: NonEmptyChain[SlotData] =>
        withMock {
          val reputationAggregator = mock[ReputationAggregatorActor[F]]

          val requestsProxy = mock[RequestsProxyActor[F]]

          val localChain = mock[LocalChainAlgebra[F]]
          val currentSlotDataHead = arbitrarySlotData.arbitrary.first
          (() => localChain.head).expects().once().onCall(() => currentSlotDataHead.pure[F])

          val slotDataStore = mock[Store[F, BlockId, SlotData]]
          val headerStore = mock[Store[F, BlockId, BlockHeader]]
          val bodyStore = mock[Store[F, BlockId, BlockBody]]
          val headerValidation = mock[BlockHeaderValidationAlgebra[F]]
          val bodySyntaxValidation = mock[BodySyntaxValidationAlgebra[F]]
          val bodySemanticValidation = mock[BodySemanticValidationAlgebra[F]]
          val bodyAuthorizationValidation = mock[BodyAuthorizationValidationAlgebra[F]]
          val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, SlotData]]

          (chainSelectionAlgebra.compare _).expects(slotData.last, currentSlotDataHead).returning((-1).pure[F])

          BlockChecker
            .makeActor(
              reputationAggregator,
              requestsProxy,
              localChain,
              slotDataStore,
              headerStore,
              bodyStore,
              headerValidation,
              bodySyntaxValidation,
              bodySemanticValidation,
              bodyAuthorizationValidation,
              chainSelectionAlgebra
            )
            .use { actor =>
              for {
                _ <- actor.sendNoWait(BlockChecker.Message.RemoteSlotData(hostId, slotData))
              } yield ()
            }
        }
    }
  }

  test("RemoteSlotData: Request headers if new slot data is better and is extension of current chain") {
    PropF.forAllF(arbitraryLinkedSlotDataChainFor(Gen.choose(3, maxChainSize)).arbitrary) {
      slotData: NonEmptyChain[SlotData] =>
        withMock {
          val reputationAggregator = mock[ReputationAggregatorActor[F]]
          val requestsProxy = mock[RequestsProxyActor[F]]
          val localChain = mock[LocalChainAlgebra[F]]
          val slotDataStore = mock[Store[F, BlockId, SlotData]]
          val headerStore = mock[Store[F, BlockId, BlockHeader]]
          val bodyStore = mock[Store[F, BlockId, BlockBody]]
          val headerValidation = mock[BlockHeaderValidationAlgebra[F]]
          val bodySyntaxValidation = mock[BodySyntaxValidationAlgebra[F]]
          val bodySemanticValidation = mock[BodySemanticValidationAlgebra[F]]
          val bodyAuthorizationValidation = mock[BodyAuthorizationValidationAlgebra[F]]
          val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, SlotData]]

          val idAndSlotData: NonEmptyChain[(BlockId, SlotData)] = slotData.map(s => (s.slotId.blockId, s))

          // local is known data which are stored in stores
          val (localId, localSlotData) = idAndSlotData.head
          (slotDataStore
            .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
            .expects(localId, *, *)
            .once()
            .returning(localSlotData.pure[F])
          (headerStore.contains _).expects(localId).once().returning(true.pure[F])

          // slot data from peer
          val remoteSlotDataAndId = NonEmptyChain.fromChain(idAndSlotData.tail).get
          val (_, remoteSlotData) = remoteSlotDataAndId.unzip

          (chainSelectionAlgebra.compare _).expects(slotData.last, localSlotData).returning(1.pure[F])

          val localSlotDataStore = idAndSlotData.toList.toMap
          (slotDataStore
            .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
            .expects(*, *, *)
            .anyNumberOfTimes()
            .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
              localSlotDataStore(id).pure[F]
            }

          (headerStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId => (id == localId).pure[F] }

          val expectedRequest =
            RequestsProxy.Message.DownloadHeadersRequest(
              hostId,
              NonEmptyChain.fromSeq(remoteSlotDataAndId.toChain.toList.take(chunkSize).map(_._1)).get
            ): RequestsProxy.Message
          (requestsProxy.sendNoWait _).expects(expectedRequest).once().returning(().pure[F])

          BlockChecker
            .makeActor(
              reputationAggregator,
              requestsProxy,
              localChain,
              slotDataStore,
              headerStore,
              bodyStore,
              headerValidation,
              bodySyntaxValidation,
              bodySemanticValidation,
              bodyAuthorizationValidation,
              chainSelectionAlgebra,
              Option(BestChain(NonEmptyChain.one(localSlotData)))
            )
            .use { actor =>
              for {
                updatedState <- actor.send(BlockChecker.Message.RemoteSlotData(hostId, remoteSlotData))
                _ = assert(updatedState.bestKnownRemoteSlotDataOpt == Option(BestChain(remoteSlotData)))
              } yield ()
            }
        }
    }
  }

  test("RemoteSlotData: Request headers if new slot data is better and is overlapped of current chain") {
    PropF.forAllF(arbitraryLinkedSlotDataChainFor(Gen.choose(3, maxChainSize)).arbitrary) {
      slotData: NonEmptyChain[SlotData] =>
        withMock {
          val reputationAggregator = mock[ReputationAggregatorActor[F]]
          val requestsProxy = mock[RequestsProxyActor[F]]
          val localChain = mock[LocalChainAlgebra[F]]
          val slotDataStore = mock[Store[F, BlockId, SlotData]]
          val headerStore = mock[Store[F, BlockId, BlockHeader]]
          val bodyStore = mock[Store[F, BlockId, BlockBody]]
          val headerValidation = mock[BlockHeaderValidationAlgebra[F]]
          val bodySyntaxValidation = mock[BodySyntaxValidationAlgebra[F]]
          val bodySemanticValidation = mock[BodySemanticValidationAlgebra[F]]
          val bodyAuthorizationValidation = mock[BodyAuthorizationValidationAlgebra[F]]
          val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, SlotData]]

          val idAndSlotData: NonEmptyChain[(BlockId, SlotData)] = slotData.map(s => (s.slotId.blockId, s))

          // local is known data which are stored in stores
          val (localId, localSlotData) = idAndSlotData.head
          (slotDataStore
            .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
            .expects(localId, *, *)
            .once()
            .returning(localSlotData.pure[F])
          (headerStore.contains _).expects(localId).once().returning(true.pure[F])

          // slot data from peer
          val remoteSlotDataAndId = NonEmptyChain.fromChain(idAndSlotData.tail).get
          val (_, remoteSlotData) = remoteSlotDataAndId.unzip

          // known but not accepted yet data
          val knownSlotData = NonEmptyChain.fromSeq(slotData.toChain.toList.take(2)).get

          (chainSelectionAlgebra.compare _).expects(slotData.last, knownSlotData.last).returning(1.pure[F])

          val localSlotDataStore = idAndSlotData.toList.toMap
          (slotDataStore
            .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
            .expects(*, *, *)
            .anyNumberOfTimes()
            .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
              localSlotDataStore(id).pure[F]
            }

          (headerStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId => (id == localId).pure[F] }

          val expectedRequest =
            RequestsProxy.Message.DownloadHeadersRequest(
              hostId,
              NonEmptyChain.fromSeq(remoteSlotDataAndId.toChain.toList.take(chunkSize).map(_._1)).get
            ): RequestsProxy.Message
          (requestsProxy.sendNoWait _).expects(expectedRequest).once().returning(().pure[F])

          BlockChecker
            .makeActor(
              reputationAggregator,
              requestsProxy,
              localChain,
              slotDataStore,
              headerStore,
              bodyStore,
              headerValidation,
              bodySyntaxValidation,
              bodySemanticValidation,
              bodyAuthorizationValidation,
              chainSelectionAlgebra,
              Option(BestChain(NonEmptyChain.one(remoteSlotData.head)))
            )
            .use { actor =>
              for {
                updatedState <- actor.send(BlockChecker.Message.RemoteSlotData(hostId, remoteSlotData))
                _ = assert(updatedState.bestKnownRemoteSlotDataOpt == Option(BestChain(remoteSlotData)))
              } yield ()
            }
        }
    }
  }

  test("RemoteSlotData: Request headers if new slot data is better and is not extension of current chain") {
    PropF.forAllF(arbitraryLinkedSlotDataChainFor(Gen.choose(3, maxChainSize)).arbitrary) {
      slotData: NonEmptyChain[SlotData] =>
        withMock {
          val reputationAggregator = mock[ReputationAggregatorActor[F]]
          val requestsProxy = mock[RequestsProxyActor[F]]
          val localChain = mock[LocalChainAlgebra[F]]
          val slotDataStore = mock[Store[F, BlockId, SlotData]]
          val headerStore = mock[Store[F, BlockId, BlockHeader]]
          val bodyStore = mock[Store[F, BlockId, BlockBody]]
          val headerValidation = mock[BlockHeaderValidationAlgebra[F]]
          val bodySyntaxValidation = mock[BodySyntaxValidationAlgebra[F]]
          val bodySemanticValidation = mock[BodySemanticValidationAlgebra[F]]
          val bodyAuthorizationValidation = mock[BodyAuthorizationValidationAlgebra[F]]
          val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, SlotData]]

          val idAndSlotData: NonEmptyChain[(BlockId, SlotData)] = slotData.map(s => (s.slotId.blockId, s))

          // local is known data which are stored in stores
          val (localId, localSlotData) = idAndSlotData.head
          (slotDataStore
            .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
            .expects(localId, *, *)
            .once()
            .returning(localSlotData.pure[F])

          // slot data from peer
          val remoteSlotDataAndId = NonEmptyChain.fromChain(idAndSlotData.tail).get
          val (_, remoteSlotData) = remoteSlotDataAndId.unzip

          (() => localChain.head).expects().once().returning(localSlotData.pure[F])

          (chainSelectionAlgebra.compare _).expects(slotData.last, localSlotData).returning(1.pure[F])

          val localSlotDataStore = idAndSlotData.toList.toMap
          (slotDataStore
            .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
            .expects(*, *, *)
            .anyNumberOfTimes()
            .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
              localSlotDataStore(id).pure[F]
            }

          (headerStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId => (id == localId).pure[F] }

          val expectedRequest =
            RequestsProxy.Message.DownloadHeadersRequest(
              hostId,
              NonEmptyChain.fromSeq(remoteSlotDataAndId.toChain.toList.take(chunkSize).map(_._1)).get
            ): RequestsProxy.Message
          (requestsProxy.sendNoWait _).expects(expectedRequest).once().returning(().pure[F])

          val expectedBestChain = Option(BestChain(NonEmptyChain.fromChain(slotData.tail).get))

          BlockChecker
            .makeActor(
              reputationAggregator,
              requestsProxy,
              localChain,
              slotDataStore,
              headerStore,
              bodyStore,
              headerValidation,
              bodySyntaxValidation,
              bodySemanticValidation,
              bodyAuthorizationValidation,
              chainSelectionAlgebra
            )
            .use { actor =>
              for {
                updatedState <- actor.send(BlockChecker.Message.RemoteSlotData(hostId, remoteSlotData))
                _ = assert(updatedState.bestKnownRemoteSlotDataOpt == expectedBestChain)
              } yield ()
            }
        }
    }
  }

  test("RemoteSlotData: Request headers if new slot data is better, building full chain") {
    PropF.forAllF(arbitraryLinkedSlotDataChainFor(Gen.choose(3, maxChainSize)).arbitrary) {
      slotData: NonEmptyChain[SlotData] =>
        withMock {
          val reputationAggregator = mock[ReputationAggregatorActor[F]]
          mock[PeersManagerActor[F]]
          val requestsProxy = mock[RequestsProxyActor[F]]
          val localChain = mock[LocalChainAlgebra[F]]
          val slotDataStore = mock[Store[F, BlockId, SlotData]]
          val headerStore = mock[Store[F, BlockId, BlockHeader]]
          val bodyStore = mock[Store[F, BlockId, BlockBody]]
          val headerValidation = mock[BlockHeaderValidationAlgebra[F]]
          val bodySyntaxValidation = mock[BodySyntaxValidationAlgebra[F]]
          val bodySemanticValidation = mock[BodySemanticValidationAlgebra[F]]
          val bodyAuthorizationValidation = mock[BodyAuthorizationValidationAlgebra[F]]
          val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, SlotData]]

          val idAndSlotData: NonEmptyChain[(BlockId, SlotData)] = slotData.map(s => (s.slotId.blockId, s))

          // local is known data which are stored in stores
          val (localId, _) = idAndSlotData.head
          (headerStore.contains _).expects(localId).once().returning(true.pure[F])

          // slot data from peer
          val remoteSize = Gen.choose[Int](2, idAndSlotData.tail.size.toInt - 1).first
          val (missedInChainData, remoteIdAndSlotData) = idAndSlotData.tail.toList.splitAt(remoteSize)
          val (_, peerSlotData) = remoteIdAndSlotData.unzip

          val localSlotDataStore = idAndSlotData.toList.toMap
          (slotDataStore
            .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
            .expects(*, *, *)
            .anyNumberOfTimes()
            .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
              localSlotDataStore(id).pure[F]
            }

          (headerStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId => (id == localId).pure[F] }

          val currentBestChain = NonEmptyChain.fromSeq(slotData.toList.take(2)).get
          (chainSelectionAlgebra.compare _).expects(slotData.last, currentBestChain.last).returning(1.pure[F])

          val expectedRequest =
            RequestsProxy.Message.DownloadHeadersRequest(
              hostId,
              NonEmptyChain.fromSeq(missedInChainData.take(chunkSize).map(_._1)).get
            ): RequestsProxy.Message
          (requestsProxy.sendNoWait _).expects(expectedRequest).once().returning(().pure[F])

          val expectedBestChain = Option(BestChain(NonEmptyChain.fromChain(slotData.tail).get))

          BlockChecker
            .makeActor(
              reputationAggregator,
              requestsProxy,
              localChain,
              slotDataStore,
              headerStore,
              bodyStore,
              headerValidation,
              bodySyntaxValidation,
              bodySemanticValidation,
              bodyAuthorizationValidation,
              chainSelectionAlgebra,
              Option(BestChain(currentBestChain))
            )
            .use { actor =>
              for {
                updatedState <- actor.send(
                  BlockChecker.Message.RemoteSlotData(hostId, NonEmptyChain.fromSeq(peerSlotData).get)
                )
                _ = assert(updatedState.bestKnownRemoteSlotDataOpt == expectedBestChain)
              } yield ()
            }
        }
    }
  }

  test("RemoteBlockHeader: Do not verify already known headers") {
    PropF.forAllF(nonEmptyChainArbOfLen(arbitraryHeader, maxChainSize).arbitrary) {
      headers: NonEmptyChain[BlockHeader] =>
        withMock {
          val reputationAggregator = mock[ReputationAggregatorActor[F]]
          val requestsProxy = mock[RequestsProxyActor[F]]
          val localChain = mock[LocalChainAlgebra[F]]
          val slotDataStore = mock[Store[F, BlockId, SlotData]]
          val headerStore = mock[Store[F, BlockId, BlockHeader]]
          val bodyStore = mock[Store[F, BlockId, BlockBody]]
          val headerValidation = mock[BlockHeaderValidationAlgebra[F]]
          val bodySyntaxValidation = mock[BodySyntaxValidationAlgebra[F]]
          val bodySemanticValidation = mock[BodySemanticValidationAlgebra[F]]
          val bodyAuthorizationValidation = mock[BodyAuthorizationValidationAlgebra[F]]
          val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, SlotData]]

          (headerStore.contains _).expects(*).rep(headers.size.toInt).returning(true.pure[F])
          (headerValidation.validate _).expects(*).never()

          val message = headers.map(UnverifiedBlockHeader(hostId, _))
          BlockChecker
            .makeActor(
              reputationAggregator,
              requestsProxy,
              localChain,
              slotDataStore,
              headerStore,
              bodyStore,
              headerValidation,
              bodySyntaxValidation,
              bodySemanticValidation,
              bodyAuthorizationValidation,
              chainSelectionAlgebra
            )
            .use { actor =>
              for {
                _ <- actor.sendNoWait(BlockChecker.Message.RemoteBlockHeaders(message))
              } yield ()
            }
        }
    }
  }

  test("RemoteBlockHeader: Do not verify already known headers, stop processing if all headers are known") {
    PropF.forAllF(arbitraryLinkedBlockHeaderChain(Gen.choose(2, maxChainSize)).arbitrary) {
      headers: NonEmptyChain[BlockHeader] =>
        withMock {
          val reputationAggregator = mock[ReputationAggregatorActor[F]]
          val requestsProxy = mock[RequestsProxyActor[F]]
          val localChain = mock[LocalChainAlgebra[F]]
          val slotDataStore = mock[Store[F, BlockId, SlotData]]
          val headerStore = mock[Store[F, BlockId, BlockHeader]]
          val bodyStore = mock[Store[F, BlockId, BlockBody]]
          val headerValidation = mock[BlockHeaderValidationAlgebra[F]]
          val bodySyntaxValidation = mock[BodySyntaxValidationAlgebra[F]]
          val bodySemanticValidation = mock[BodySemanticValidationAlgebra[F]]
          val bodyAuthorizationValidation = mock[BodyAuthorizationValidationAlgebra[F]]
          val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, SlotData]]

          (headerStore.contains _).expects(*).rep(headers.size.toInt).returning(true.pure[F])
          (headerValidation.validate _).expects(*).never()

          val message = headers.map(UnverifiedBlockHeader(hostId, _))

          BlockChecker
            .makeActor(
              reputationAggregator,
              requestsProxy,
              localChain,
              slotDataStore,
              headerStore,
              bodyStore,
              headerValidation,
              bodySyntaxValidation,
              bodySemanticValidation,
              bodyAuthorizationValidation,
              chainSelectionAlgebra
            )
            .use { actor =>
              for {
                _ <- actor.sendNoWait(BlockChecker.Message.RemoteBlockHeaders(message))
              } yield ()
            }
        }
    }
  }

  test("RemoteBlockHeader: Do not verify already known headers, request bodies, do not request next headers") {
    PropF.forAllF(arbitraryLinkedBlockHeaderChain(Gen.choose(2, maxChainSize)).arbitrary) {
      headers: NonEmptyChain[BlockHeader] =>
        withMock {
          val reputationAggregator = mock[ReputationAggregatorActor[F]]
          val requestsProxy = mock[RequestsProxyActor[F]]
          val localChain = mock[LocalChainAlgebra[F]]
          val slotDataStore = mock[Store[F, BlockId, SlotData]]
          val headerStore = mock[Store[F, BlockId, BlockHeader]]
          val bodyStore = mock[Store[F, BlockId, BlockBody]]
          val headerValidation = mock[BlockHeaderValidationAlgebra[F]]
          val bodySyntaxValidation = mock[BodySyntaxValidationAlgebra[F]]
          val bodySemanticValidation = mock[BodySemanticValidationAlgebra[F]]
          val bodyAuthorizationValidation = mock[BodyAuthorizationValidationAlgebra[F]]
          val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, SlotData]]

          val idAndHeaders = headers.map(h => (h.id, h))

          val knownHeadersSize = Gen.choose[Int](1, headers.size.toInt - 1).first
          val (knownIdAndHeaders, newIdAndHeaders) = idAndHeaders.toList.splitAt(knownHeadersSize)

          val headerStoreData = mutable.Map.empty[BlockId, BlockHeader] ++ knownIdAndHeaders.toMap
          (headerStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
            headerStoreData.contains(id).pure[F]
          }
          (headerStore.get _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
            headerStoreData.get(id).pure[F]
          }
          (headerStore
            .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
            .expects(*, *, *)
            .anyNumberOfTimes()
            .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
              headerStoreData(id).pure[F]
            }
          val addedHeader = mutable.Map.empty[BlockId, BlockHeader]
          (headerStore.put _).expects(*, *).rep(newIdAndHeaders.size).onCall {
            case (id: BlockId, header: BlockHeader) =>
              addedHeader.put(id, header)
              headerStoreData.put(id, header)
              ().pure[F]
          }

          (headerValidation.validate _)
            .expects(*)
            .rep(newIdAndHeaders.size)
            .onCall((header: BlockHeader) => Either.right[BlockHeaderValidationFailure, BlockHeader](header).pure[F])

          val bodyStoreData = idAndHeaders.toList.toMap
          (bodyStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
            (!bodyStoreData.contains(id)).pure[F]
          }

          (slotDataStore
            .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
            .expects(*, *, *)
            .rep(idAndHeaders.size.toInt)
            .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
              val header = headerStoreData(id)
              val arbSlotData = arbitrarySlotData.arbitrary.first
              val slotData =
                arbSlotData.copy(
                  slotId = arbSlotData.slotId.copy(blockId = header.id),
                  parentSlotId = arbSlotData.parentSlotId.copy(blockId = header.parentHeaderId)
                )
              slotData.pure[F]
            }

          val expectedHeaders = NonEmptyChain.fromSeq(idAndHeaders.toList.take(chunkSize).map(_._2)).get
          val expectedMessage: RequestsProxy.Message =
            RequestsProxy.Message.DownloadBodiesRequest(hostId, expectedHeaders)
          (requestsProxy.sendNoWait _).expects(expectedMessage).returning(().pure[F])

          val message = headers.map(UnverifiedBlockHeader(hostId, _))

          BlockChecker
            .makeActor(
              reputationAggregator,
              requestsProxy,
              localChain,
              slotDataStore,
              headerStore,
              bodyStore,
              headerValidation,
              bodySyntaxValidation,
              bodySemanticValidation,
              bodyAuthorizationValidation,
              chainSelectionAlgebra
            )
            .use { actor =>
              for {
                _ <- actor.send(BlockChecker.Message.RemoteBlockHeaders(message))
                _ = assert(newIdAndHeaders.map(_._1).forall(k => addedHeader.contains(k)))
              } yield ()
            }
        }
    }
  }

  test("RemoteBlockHeader: Do not verify known headers, request bodies, request next headers because best chain") {
    PropF.forAllF(arbitraryLinkedBlockHeaderChain(Gen.choose(2, maxChainSize)).arbitrary) {
      headers: NonEmptyChain[BlockHeader] =>
        withMock {
          val reputationAggregator = mock[ReputationAggregatorActor[F]]
          val requestsProxy = mock[RequestsProxyActor[F]]
          val localChain = mock[LocalChainAlgebra[F]]
          val slotDataStore = mock[Store[F, BlockId, SlotData]]
          val headerStore = mock[Store[F, BlockId, BlockHeader]]
          val bodyStore = mock[Store[F, BlockId, BlockBody]]
          val headerValidation = mock[BlockHeaderValidationAlgebra[F]]
          val bodySyntaxValidation = mock[BodySyntaxValidationAlgebra[F]]
          val bodySemanticValidation = mock[BodySemanticValidationAlgebra[F]]
          val bodyAuthorizationValidation = mock[BodyAuthorizationValidationAlgebra[F]]
          val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, SlotData]]

          val idAndHeaders = headers.map(h => (h.id, h))

          val knownHeadersSize = Gen.choose[Int](1, headers.size.toInt - 1).first
          val (knownIdAndHeaders, newIdAndHeaders) = idAndHeaders.toList.splitAt(knownHeadersSize)

          val headerStoreData = mutable.Map.empty[BlockId, BlockHeader] ++ knownIdAndHeaders.toMap
          (headerStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
            headerStoreData.contains(id).pure[F]
          }
          (headerStore.get _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
            headerStoreData.get(id).pure[F]
          }
          (headerStore
            .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
            .expects(*, *, *)
            .anyNumberOfTimes()
            .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
              headerStoreData(id).pure[F]
            }
          val addedHeader = mutable.Map.empty[BlockId, BlockHeader]
          (headerStore.put _).expects(*, *).rep(newIdAndHeaders.size).onCall {
            case (id: BlockId, header: BlockHeader) =>
              addedHeader.put(id, header)
              headerStoreData.put(id, header)
              ().pure[F]
          }

          (headerValidation.validate _)
            .expects(*)
            .rep(newIdAndHeaders.size)
            .onCall((header: BlockHeader) => Either.right[BlockHeaderValidationFailure, BlockHeader](header).pure[F])

          val bodyStoreData = idAndHeaders.toList.toMap
          (bodyStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
            (!bodyStoreData.contains(id)).pure[F]
          }

          (slotDataStore
            .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
            .expects(*, *, *)
            .rep(idAndHeaders.size.toInt)
            .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
              val header = headerStoreData(id)
              val arbSlotData = arbitrarySlotData.arbitrary.first
              val slotData =
                arbSlotData.copy(
                  slotId = arbSlotData.slotId.copy(blockId = header.id),
                  parentSlotId = arbSlotData.parentSlotId.copy(blockId = header.parentHeaderId)
                )
              slotData.pure[F]
            }

          val expectedHeaders = NonEmptyChain.fromSeq(idAndHeaders.toList.take(chunkSize).map(_._2)).get
          val expectedMessage: RequestsProxy.Message =
            RequestsProxy.Message.DownloadBodiesRequest(hostId, expectedHeaders)
          (requestsProxy.sendNoWait _).expects(expectedMessage).returning(().pure[F])

          val bestChainForKnownAndNewIds: NonEmptyChain[SlotData] =
            idAndHeaders.map { case (id, header) =>
              val parentId = header.parentHeaderId
              val slotId = SlotId(blockId = id)
              val parentSlotId = SlotId(blockId = parentId)
              arbitrarySlotData.arbitrary.first.copy(slotId = slotId, parentSlotId = parentSlotId)
            }

          val newSlotData: Chain[SlotData] = {
            val arSlot = arbitraryLinkedSlotDataChain.arbitrary.retryUntil(c => c.size > 1 && c.size < 10).first
            arSlot.head.copy(parentSlotId = bestChainForKnownAndNewIds.last.slotId) +: arSlot.tail
          }

          val bestChain = bestChainForKnownAndNewIds.appendChain(newSlotData)

          val idAndSlotData = bestChain.map(s => (s.slotId.blockId, s))
          val localSlotDataStore = idAndSlotData.toList.toMap
          (slotDataStore
            .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
            .expects(*, *, *)
            .anyNumberOfTimes()
            .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
              localSlotDataStore(id).pure[F]
            }

          val nextHeaders = newSlotData.toList.take(chunkSize).map(s => s.slotId.blockId)

          val nextHeaderMessage: RequestsProxy.Message =
            RequestsProxy.Message.DownloadHeadersRequest(hostId, NonEmptyChain.fromSeq(nextHeaders).get)
          (requestsProxy.sendNoWait _).expects(nextHeaderMessage).once().returning(().pure[F])

          val message = headers.map(UnverifiedBlockHeader(hostId, _))

          BlockChecker
            .makeActor(
              reputationAggregator,
              requestsProxy,
              localChain,
              slotDataStore,
              headerStore,
              bodyStore,
              headerValidation,
              bodySyntaxValidation,
              bodySemanticValidation,
              bodyAuthorizationValidation,
              chainSelectionAlgebra,
              Option(BestChain(bestChain)),
              Option(hostId)
            )
            .use { actor =>
              for {
                _ <- actor.send(BlockChecker.Message.RemoteBlockHeaders(message))
                _ = assert(newIdAndHeaders.map(_._1).forall(k => addedHeader.contains(k)))
              } yield ()
            }
        }
    }
  }

  test("RemoteBlockHeader: Do not verify already known headers, apply only first header, second is not correct") {
    PropF.forAllF(arbitraryLinkedBlockHeaderChain(Gen.choose(3, maxChainSize)).arbitrary) {
      headers: NonEmptyChain[BlockHeader] =>
        withMock {
          val reputationAggregator = mock[ReputationAggregatorActor[F]]
          val requestsProxy = mock[RequestsProxyActor[F]]
          val localChain = mock[LocalChainAlgebra[F]]
          val slotDataStore = mock[Store[F, BlockId, SlotData]]
          val headerStore = mock[Store[F, BlockId, BlockHeader]]
          val bodyStore = mock[Store[F, BlockId, BlockBody]]
          val headerValidation = mock[BlockHeaderValidationAlgebra[F]]
          val bodySyntaxValidation = mock[BodySyntaxValidationAlgebra[F]]
          val bodySemanticValidation = mock[BodySemanticValidationAlgebra[F]]
          val bodyAuthorizationValidation = mock[BodyAuthorizationValidationAlgebra[F]]
          val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, SlotData]]

          val idAndHeaders = headers.map(h => (h.id, h))

          val knownHeadersSize = Gen.choose[Int](1, headers.size.toInt - 2).first
          val (knownIdAndHeaders, newIdAndHeaders) = idAndHeaders.toList.splitAt(knownHeadersSize)

          val headerStoreData = mutable.Map.empty[BlockId, BlockHeader] ++ knownIdAndHeaders.toMap
          (headerStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
            headerStoreData.contains(id).pure[F]
          }
          (headerStore.get _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
            headerStoreData.get(id).pure[F]
          }
          (headerStore
            .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
            .expects(*, *, *)
            .anyNumberOfTimes()
            .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
              headerStoreData(id).pure[F]
            }
          val addedHeader = mutable.Map.empty[BlockId, BlockHeader]
          (headerStore.put _).expects(*, *).rep(1).onCall { case (id: BlockId, header: BlockHeader) =>
            addedHeader.put(id, header)
            headerStoreData.put(id, header)
            ().pure[F]
          }

          (headerValidation.validate _)
            .expects(*)
            .once()
            .onCall((header: BlockHeader) => Either.right[BlockHeaderValidationFailure, BlockHeader](header).pure[F])
          (headerValidation.validate _)
            .expects(*)
            .once()
            .onCall((_: BlockHeader) =>
              Either.left[BlockHeaderValidationFailure, BlockHeader](NonForwardSlot(0, 1)).pure[F]
            )

          val bodyStoreData = idAndHeaders.toList.toMap
          (bodyStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
            (!bodyStoreData.contains(id)).pure[F]
          }

          (slotDataStore
            .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
            .expects(*, *, *)
            .anyNumberOfTimes()
            .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
              val header = headerStoreData(id)
              val arbSlotData = arbitrarySlotData.arbitrary.first
              val slotData =
                arbSlotData.copy(
                  slotId = arbSlotData.slotId.copy(blockId = header.id),
                  parentSlotId = arbSlotData.parentSlotId.copy(blockId = header.parentHeaderId)
                )
              slotData.pure[F]
            }

          val expectedHeaders = NonEmptyChain.fromSeq(idAndHeaders.toList.take(chunkSize).map(_._2)).get
          val expectedMessage: RequestsProxy.Message =
            RequestsProxy.Message.DownloadBodiesRequest(hostId, expectedHeaders)
          (requestsProxy.sendNoWait _)
            .expects(expectedMessage)
            .returning(().pure[F])
          (requestsProxy.sendNoWait _)
            .expects(RequestsProxy.Message.InvalidateBlockId(hostId, newIdAndHeaders(1)._1))
            .returning(().pure[F])
          (requestsProxy.sendNoWait _)
            .expects(RequestsProxy.Message.GetCurrentTips)
            .returns(().pure[F])

          val message = headers.map(UnverifiedBlockHeader(hostId, _))

          BlockChecker
            .makeActor(
              reputationAggregator,
              requestsProxy,
              localChain,
              slotDataStore,
              headerStore,
              bodyStore,
              headerValidation,
              bodySyntaxValidation,
              bodySemanticValidation,
              bodyAuthorizationValidation,
              chainSelectionAlgebra,
              bestChain = Option(BestChain(NonEmptyChain.fromSeq(newIdAndHeaders.map(d => headerToSlotData(d._2))).get))
            )
            .use { actor =>
              for {
                state <- actor.send(BlockChecker.Message.RemoteBlockHeaders(message))
                _ = assert(addedHeader.contains(newIdAndHeaders.head._1))
                _ = assert(state.bestKnownRemoteSlotDataOpt.isEmpty)
                _ = assert(state.bestKnownRemoteSlotDataHost.isEmpty)
              } yield ()
            }
        }
    }
  }

  test("RemoteBlockBodies: Skip verification already known bodies") {
    PropF.forAllF(
      nonEmptyChainArbOfLen(ModelGenerators.arbitraryNodeBody, Gen.choose(2, maxChainSize).first).arbitrary
    ) { bodies: NonEmptyChain[BlockBody] =>
      withMock {
        val reputationAggregator = mock[ReputationAggregatorActor[F]]
        val requestsProxy = mock[RequestsProxyActor[F]]
        val localChain = mock[LocalChainAlgebra[F]]
        val slotDataStore = mock[Store[F, BlockId, SlotData]]
        val headerStore = mock[Store[F, BlockId, BlockHeader]]
        val bodyStore = mock[Store[F, BlockId, BlockBody]]
        val headerValidation = mock[BlockHeaderValidationAlgebra[F]]
        val bodySyntaxValidation = mock[BodySyntaxValidationAlgebra[F]]
        val bodySemanticValidation = mock[BodySemanticValidationAlgebra[F]]
        val bodyAuthorizationValidation = mock[BodyAuthorizationValidationAlgebra[F]]
        val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, SlotData]]

        val ids: NonEmptyChain[BlockHeader] = bodies.map(_ => arbitraryHeader.arbitrary.first)

        val blocks: NonEmptyChain[Block] =
          ids.zipWith(bodies)((_, _)).map { case (header, body) => Block(header, body) }

        val message =
          BlockChecker.Message.RemoteBlockBodies(blocks.map(d => (d.header, UnverifiedBlockBody(hostId, d.body))))

        (bodyStore.contains _).expects(*).rep(bodies.size.toInt).returning(true.pure[F])

        BlockChecker
          .makeActor(
            reputationAggregator,
            requestsProxy,
            localChain,
            slotDataStore,
            headerStore,
            bodyStore,
            headerValidation,
            bodySyntaxValidation,
            bodySemanticValidation,
            bodyAuthorizationValidation,
            chainSelectionAlgebra
          )
          .use { actor =>
            for {
              _ <- actor.send(message)
            } yield ()
          }
      }
    }
  }

  test("RemoteBlockBodies: Verify and save new blocks, but not apply to local chain because it worse") {
    PropF.forAllF(
      nonEmptyChainArbOfLen(ModelGenerators.arbitraryNodeBody, Gen.choose(2, maxChainSize).first).arbitrary
    ) { bodies: NonEmptyChain[BlockBody] =>
      withMock {
        val reputationAggregator = mock[ReputationAggregatorActor[F]]
        val requestsProxy = mock[RequestsProxyActor[F]]
        val localChain = mock[LocalChainAlgebra[F]]
        val slotDataStore = mock[Store[F, BlockId, SlotData]]
        val headerStore = mock[Store[F, BlockId, BlockHeader]]
        val bodyStore = mock[Store[F, BlockId, BlockBody]]
        val headerValidation = mock[BlockHeaderValidationAlgebra[F]]
        val bodySyntaxValidation = mock[BodySyntaxValidationAlgebra[F]]
        val bodySemanticValidation = mock[BodySemanticValidationAlgebra[F]]
        val bodyAuthorizationValidation = mock[BodyAuthorizationValidationAlgebra[F]]
        val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, SlotData]]

        val headers: NonEmptyChain[BlockHeader] = bodies.map(_ => arbitraryHeader.arbitrary.first).map(_.embedId)

        val blocks: NonEmptyChain[Block] =
          headers.zipWith(bodies)((_, _)).map { case (header, body) => Block(header, body) }
        val idAndHeader: NonEmptyChain[(BlockId, BlockHeader)] = headers.map(h => (h.id, h))
        val idAndBody = blocks.map(block => (block.header.id, block.body))

        val message =
          BlockChecker.Message.RemoteBlockBodies(blocks.map(d => (d.header, UnverifiedBlockBody(hostId, d.body))))

        val knownBodiesSize = Gen.choose[Int](1, bodies.size.toInt - 1).first
        val (knownIdAndHeaders, newIdAndHeaders) = idAndBody.toList.splitAt(knownBodiesSize)

        val newBodiesSize = newIdAndHeaders.size

        val knownBodyStorageData = knownIdAndHeaders.toMap
        (bodyStore.contains _).expects(*).rep(bodies.size.toInt).onCall { id: BlockId =>
          knownBodyStorageData.contains(id).pure[F]
        }

        val headerStorageData = idAndHeader.toChain.toList.toMap
        (headerStore
          .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
          .expects(*, *, *)
          .anyNumberOfTimes()
          .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
            headerStorageData(id).pure[F]
          }

        (bodySyntaxValidation.validate _).expects(*).rep(newBodiesSize).onCall { b: BlockBody =>
          Validated.validNec[BodySyntaxError, BlockBody](b).pure[F]
        }

        (bodySemanticValidation
          .validate(_: BodyValidationContext)(_: BlockBody))
          .expects(*, *)
          .rep(newBodiesSize)
          .onCall { case (_: BodyValidationContext, b: BlockBody) =>
            Validated.validNec[BodySemanticError, BlockBody](b).pure[F]
          }

        type AuthContext = IoTransaction => DynamicContext[F, String, Datum]
        (bodyAuthorizationValidation
          .validate(_: AuthContext)(_: BlockBody))
          .expects(*, *)
          .rep(newBodiesSize)
          .onCall { case (_: AuthContext @unchecked, b: BlockBody) =>
            Validated.validNec[BodyAuthorizationError, BlockBody](b).pure[F]
          }

        val storedBodies = mutable.Map.empty[BlockId, BlockBody]
        (bodyStore.put _).expects(*, *).rep(newBodiesSize).onCall { case (id: BlockId, block: BlockBody) =>
          storedBodies.put(id, block).pure[F].void
        }

        val slotsStorageData = idAndHeader.map { case (id, header) => (id, headerToSlotData(header)) }.toList.toMap
        (slotDataStore.get _).expects(*).anyNumberOfTimes().onCall { id: BlockId => slotsStorageData.get(id).pure[F] }
        (slotDataStore
          .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
          .expects(*, *, *)
          .anyNumberOfTimes()
          .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
            slotsStorageData(id).pure[F]
          }

        (localChain.isWorseThan _).expects(*).anyNumberOfTimes().returning(false.pure[F])

        BlockChecker
          .makeActor(
            reputationAggregator,
            requestsProxy,
            localChain,
            slotDataStore,
            headerStore,
            bodyStore,
            headerValidation,
            bodySyntaxValidation,
            bodySemanticValidation,
            bodyAuthorizationValidation,
            chainSelectionAlgebra
          )
          .use { actor =>
            for {
              _ <- actor.send(message)
            } yield ()
          }
      }
    }
  }

  test("RemoteBlockBodies: Verify and save blocks, apply to local chain, send no new request") {
    PropF.forAllF(
      nonEmptyChainArbOfLen(ModelGenerators.arbitraryNodeBody, Gen.choose(2, maxChainSize).first).arbitrary
    ) { bodies: NonEmptyChain[BlockBody] =>
      withMock {
        val reputationAggregator = mock[ReputationAggregatorActor[F]]
        val requestsProxy = mock[RequestsProxyActor[F]]
        val localChain = mock[LocalChainAlgebra[F]]
        val slotDataStore = mock[Store[F, BlockId, SlotData]]
        val headerStore = mock[Store[F, BlockId, BlockHeader]]
        val bodyStore = mock[Store[F, BlockId, BlockBody]]
        val headerValidation = mock[BlockHeaderValidationAlgebra[F]]
        val bodySyntaxValidation = mock[BodySyntaxValidationAlgebra[F]]
        val bodySemanticValidation = mock[BodySemanticValidationAlgebra[F]]
        val bodyAuthorizationValidation = mock[BodyAuthorizationValidationAlgebra[F]]
        val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, SlotData]]

        val headers: NonEmptyChain[BlockHeader] = bodies.map(_ => arbitraryHeader.arbitrary.first).map(_.embedId)

        val blocks: NonEmptyChain[Block] =
          headers.zipWith(bodies)((_, _)).map { case (header, body) => Block(header, body) }
        val idAndHeader: NonEmptyChain[(BlockId, BlockHeader)] = headers.map(h => (h.id, h))
        val idAndBody = blocks.map(block => (block.header.id, block.body))

        val message =
          BlockChecker.Message.RemoteBlockBodies(blocks.map(d => (d.header, UnverifiedBlockBody(hostId, d.body))))

        val knownBodiesSize = Gen.choose[Int](1, bodies.size.toInt - 1).first
        val (knownIdAndHeaders, newIdAndHeaders) = idAndBody.toList.splitAt(knownBodiesSize)

        val newBodiesSize = newIdAndHeaders.size

        val knownBodyStorageData = knownIdAndHeaders.toMap
        (bodyStore.contains _).expects(*).rep(bodies.size.toInt).onCall { id: BlockId =>
          knownBodyStorageData.contains(id).pure[F]
        }

        val headerStorageData = idAndHeader.toChain.toList.toMap
        (headerStore
          .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
          .expects(*, *, *)
          .anyNumberOfTimes()
          .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
            headerStorageData(id).pure[F]
          }

        (bodySyntaxValidation.validate _).expects(*).rep(newBodiesSize).onCall { b: BlockBody =>
          Validated.validNec[BodySyntaxError, BlockBody](b).pure[F]
        }

        (bodySemanticValidation
          .validate(_: BodyValidationContext)(_: BlockBody))
          .expects(*, *)
          .rep(newBodiesSize)
          .onCall { case (_: BodyValidationContext, b: BlockBody) =>
            Validated.validNec[BodySemanticError, BlockBody](b).pure[F]
          }

        type AuthContext = IoTransaction => DynamicContext[F, String, Datum]
        (bodyAuthorizationValidation
          .validate(_: AuthContext)(_: BlockBody))
          .expects(*, *)
          .rep(newBodiesSize)
          .onCall { case (_: AuthContext @unchecked, b: BlockBody) =>
            Validated.validNec[BodyAuthorizationError, BlockBody](b).pure[F]
          }

        val storedBodies = mutable.Map.empty[BlockId, BlockBody]
        (bodyStore.put _).expects(*, *).rep(newBodiesSize).onCall { case (id: BlockId, block: BlockBody) =>
          storedBodies.put(id, block).pure[F].void
        }

        val idAndSlotData = idAndHeader.map { case (id, header) => (id, headerToSlotData(header)) }.toList
        val slotsStorageData = idAndSlotData.toMap
        (slotDataStore.get _).expects(*).anyNumberOfTimes().onCall { id: BlockId => slotsStorageData.get(id).pure[F] }
        (slotDataStore
          .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
          .expects(*, *, *)
          .anyNumberOfTimes()
          .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
            slotsStorageData(id).pure[F]
          }

        val lastBlockSlotData = idAndSlotData.last._2

        (localChain.isWorseThan _).expects(*).anyNumberOfTimes().onCall { id: SlotData =>
          (lastBlockSlotData === id).pure[F]
        }

        (localChain.adopt _).expects(Validated.Valid(lastBlockSlotData)).once().returning(().pure[F])

        (bodyStore.contains _).expects(*).once().returning(true.pure[F])

        (requestsProxy.sendNoWait _).expects(*).never()

        BlockChecker
          .makeActor(
            reputationAggregator,
            requestsProxy,
            localChain,
            slotDataStore,
            headerStore,
            bodyStore,
            headerValidation,
            bodySyntaxValidation,
            bodySemanticValidation,
            bodyAuthorizationValidation,
            chainSelectionAlgebra,
            Option(BestChain(NonEmptyChain.one(lastBlockSlotData)))
          )
          .use { actor =>
            for {
              newState <- actor.send(message)
              _ = assert(newState.bestKnownRemoteSlotDataOpt.isEmpty)
            } yield ()
          }
      }
    }
  }

  test("RemoteBlockBodies: Verify and save blocks, apply to local chain, send new request due headers exist") {
    withMock {
      val reputationAggregator = mock[ReputationAggregatorActor[F]]
      val requestsProxy = mock[RequestsProxyActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val headerStore = mock[Store[F, BlockId, BlockHeader]]
      val bodyStore = mock[Store[F, BlockId, BlockBody]]
      val headerValidation = mock[BlockHeaderValidationAlgebra[F]]
      val bodySyntaxValidation = mock[BodySyntaxValidationAlgebra[F]]
      val bodySemanticValidation = mock[BodySemanticValidationAlgebra[F]]
      val bodyAuthorizationValidation = mock[BodyAuthorizationValidationAlgebra[F]]
      val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, SlotData]]

      // [1 - 5) -- known SlotData known Header known Body
      // [5 - 10) -- known SlotData known Header unknown Body
      // [10 - 15) -- known SlotData unknown Header unknown Body
      // Minimum length for allIdSlotDataHeaderBlock is 11 then
      val knownSlotHeaderBodyLen = 5
      val knownSlotHeaderUnknownBodyLen = 10
      val downloadedBodies = Gen.choose(knownSlotHeaderBodyLen + 1, knownSlotHeaderUnknownBodyLen - 1).first
      val knownSlotUnknownHeaderBodyLen = 15
      val allIdSlotDataHeaderBlock =
        arbitraryLinkedSlotDataHeaderBlockNoTx(
          Gen.choose(knownSlotUnknownHeaderBodyLen, knownSlotUnknownHeaderBodyLen)
        ).arbitrary.first.toList

      val knowSlotData =
        allIdSlotDataHeaderBlock.map(data => (data._1, data._2))

      val knownHeaders =
        allIdSlotDataHeaderBlock.take(knownSlotHeaderUnknownBodyLen).map(d => (d._1, d._3))

      val knownBody =
        allIdSlotDataHeaderBlock.take(knownSlotHeaderBodyLen).map(d => (d._1, d._4))

      val requestIdSlotDataHeaderBlock =
        allIdSlotDataHeaderBlock.slice(knownSlotHeaderBodyLen, downloadedBodies)
      val requestIdSlotDataHeaderBlockSize = requestIdSlotDataHeaderBlock.size

      val expectedNewRequest = allIdSlotDataHeaderBlock.drop(downloadedBodies)

      val messageData = NonEmptyChain.fromSeq(requestIdSlotDataHeaderBlock.map(d => Block(d._3, d._4))).get

      val message =
        BlockChecker.Message.RemoteBlockBodies(messageData.map(d => (d.header, UnverifiedBlockBody(hostId, d.body))))

      val knownBodyStorageData: mutable.Map[BlockId, BlockBody] =
        mutable.Map.empty[BlockId, BlockBody] ++ knownBody.toMap
      (bodyStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
        knownBodyStorageData.contains(id).pure[F]
      }
      (bodyStore.put _).expects(*, *).rep(requestIdSlotDataHeaderBlockSize).onCall {
        case (id: BlockId, block: BlockBody) =>
          knownBodyStorageData.put(id, block).pure[F].void
      }

      val headerStorageData = knownHeaders.toMap
      (headerStore
        .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
          headerStorageData(id).pure[F]
        }
      (headerStore.get _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
        headerStorageData.get(id).pure[F]
      }
      (headerStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
        headerStorageData.contains(id).pure[F]
      }

      (bodySyntaxValidation.validate _).expects(*).rep(requestIdSlotDataHeaderBlockSize).onCall { b: BlockBody =>
        Validated.validNec[BodySyntaxError, BlockBody](b).pure[F]
      }

      (bodySemanticValidation
        .validate(_: BodyValidationContext)(_: BlockBody))
        .expects(*, *)
        .rep(requestIdSlotDataHeaderBlockSize)
        .onCall { case (_: BodyValidationContext, b: BlockBody) =>
          Validated.validNec[BodySemanticError, BlockBody](b).pure[F]
        }

      type AuthContext = IoTransaction => DynamicContext[F, String, Datum]
      (bodyAuthorizationValidation
        .validate(_: AuthContext)(_: BlockBody))
        .expects(*, *)
        .rep(requestIdSlotDataHeaderBlockSize)
        .onCall { case (_: AuthContext @unchecked, b: BlockBody) =>
          Validated.validNec[BodyAuthorizationError, BlockBody](b).pure[F]
        }

      val slotDataStoreData = knowSlotData.toMap
      (slotDataStore.get _).expects(*).anyNumberOfTimes().onCall { id: BlockId => slotDataStoreData.get(id).pure[F] }
      (slotDataStore
        .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
          slotDataStoreData(id).pure[F]
        }

      (localChain.isWorseThan _).expects(*).rep(requestIdSlotDataHeaderBlockSize).returning(true.pure[F])
      (localChain.adopt _).expects(*).rep(requestIdSlotDataHeaderBlockSize).returning(().pure[F])

      val expectedNewRequestMessage: RequestsProxy.Message =
        RequestsProxy.Message.DownloadBodiesRequest(
          hostId,
          NonEmptyChain.fromSeq(expectedNewRequest.map(d => d._3.embedId).take(chunkSize)).get
        )
      (requestsProxy.sendNoWait _).expects(expectedNewRequestMessage).once().returning(().pure[F])

      BlockChecker
        .makeActor(
          reputationAggregator,
          requestsProxy,
          localChain,
          slotDataStore,
          headerStore,
          bodyStore,
          headerValidation,
          bodySyntaxValidation,
          bodySemanticValidation,
          bodyAuthorizationValidation,
          chainSelectionAlgebra,
          Option(BestChain(NonEmptyChain.one(allIdSlotDataHeaderBlock.last._2))),
          Option(hostId)
        )
        .use { actor =>
          for {
            newState <- actor.send(message)
            _ = assert(
              newState.bestKnownRemoteSlotDataOpt == Option(
                BestChain(NonEmptyChain.one(allIdSlotDataHeaderBlock.last._2))
              )
            )
          } yield ()
        }
    }
  }

  test("RemoteBlockBodies: Verify and save blocks, apply to local chain, send no new request due headers") {
    withMock {
      val reputationAggregator = mock[ReputationAggregatorActor[F]]
      val requestsProxy = mock[RequestsProxyActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val headerStore = mock[Store[F, BlockId, BlockHeader]]
      val bodyStore = mock[Store[F, BlockId, BlockBody]]
      val headerValidation = mock[BlockHeaderValidationAlgebra[F]]
      val bodySyntaxValidation = mock[BodySyntaxValidationAlgebra[F]]
      val bodySemanticValidation = mock[BodySemanticValidationAlgebra[F]]
      val bodyAuthorizationValidation = mock[BodyAuthorizationValidationAlgebra[F]]
      val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, SlotData]]

      // [1 - 5) -- known SlotData known Header known Body
      // [5 - 10) -- known SlotData known Header unknown Body
      // [10 - 15) -- known SlotData unknown Header unknown Body
      // Minimum length for allIdSlotDataHeaderBlock is 11 then
      val knownSlotHeaderBodyLen = 5
      val knownSlotHeaderUnknownBodyLen = 10
      val downloadedBodies = 10
      val knownSlotUnknownHeaderBodyLen = 15
      val allIdSlotDataHeaderBlock =
        arbitraryLinkedSlotDataHeaderBlockNoTx(
          Gen.choose(knownSlotUnknownHeaderBodyLen, knownSlotUnknownHeaderBodyLen)
        ).arbitrary.first.toList

      val knowSlotData =
        allIdSlotDataHeaderBlock.map(data => (data._1, data._2))

      val knownHeaders =
        allIdSlotDataHeaderBlock.take(knownSlotHeaderUnknownBodyLen).map(d => (d._1, d._3))

      val knownBody =
        allIdSlotDataHeaderBlock.take(knownSlotHeaderBodyLen).map(d => (d._1, d._4))

      val requestIdSlotDataHeaderBlock =
        allIdSlotDataHeaderBlock.slice(knownSlotHeaderBodyLen, downloadedBodies)
      val requestIdSlotDataHeaderBlockSize = requestIdSlotDataHeaderBlock.size

      val messageData =
        NonEmptyChain.fromSeq(requestIdSlotDataHeaderBlock.map(d => Block(d._3, d._4))).get

      val message =
        BlockChecker.Message.RemoteBlockBodies(messageData.map(d => (d.header, UnverifiedBlockBody(hostId, d.body))))

      val knownBodyStorageData: mutable.Map[BlockId, BlockBody] =
        mutable.Map.empty[BlockId, BlockBody] ++ knownBody.toMap
      (bodyStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
        knownBodyStorageData.contains(id).pure[F]
      }
      (bodyStore.put _).expects(*, *).rep(requestIdSlotDataHeaderBlockSize).onCall {
        case (id: BlockId, block: BlockBody) =>
          knownBodyStorageData.put(id, block).pure[F].void
      }

      val headerStorageData = knownHeaders.toMap
      (headerStore
        .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
          headerStorageData(id).pure[F]
        }
      (headerStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
        headerStorageData.contains(id).pure[F]
      }
      (headerStore.get _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
        headerStorageData.get(id).pure[F]
      }

      (bodySyntaxValidation.validate _).expects(*).rep(requestIdSlotDataHeaderBlockSize).onCall { b: BlockBody =>
        Validated.validNec[BodySyntaxError, BlockBody](b).pure[F]
      }

      (bodySemanticValidation
        .validate(_: BodyValidationContext)(_: BlockBody))
        .expects(*, *)
        .rep(requestIdSlotDataHeaderBlockSize)
        .onCall { case (_: BodyValidationContext, b: BlockBody) =>
          Validated.validNec[BodySemanticError, BlockBody](b).pure[F]
        }

      type AuthContext = IoTransaction => DynamicContext[F, String, Datum]
      (bodyAuthorizationValidation
        .validate(_: AuthContext)(_: BlockBody))
        .expects(*, *)
        .rep(requestIdSlotDataHeaderBlockSize)
        .onCall { case (_: AuthContext @unchecked, b: BlockBody) =>
          Validated.validNec[BodyAuthorizationError, BlockBody](b).pure[F]
        }

      val slotDataStoreData = knowSlotData.toMap
      (slotDataStore.get _).expects(*).anyNumberOfTimes().onCall { id: BlockId => slotDataStoreData.get(id).pure[F] }
      (slotDataStore
        .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
          slotDataStoreData(id).pure[F]
        }

      (localChain.isWorseThan _).expects(*).rep(requestIdSlotDataHeaderBlockSize).returning(true.pure[F])
      (localChain.adopt _).expects(*).rep(requestIdSlotDataHeaderBlockSize).returning(().pure[F])

      (requestsProxy.sendNoWait _).expects(*).never()

      BlockChecker
        .makeActor(
          reputationAggregator,
          requestsProxy,
          localChain,
          slotDataStore,
          headerStore,
          bodyStore,
          headerValidation,
          bodySyntaxValidation,
          bodySemanticValidation,
          bodyAuthorizationValidation,
          chainSelectionAlgebra,
          Option(BestChain(NonEmptyChain.one(allIdSlotDataHeaderBlock.last._2))),
          Option(hostId)
        )
        .use { actor =>
          for {
            newState <- actor.send(message)
            _ = assert(
              newState.bestKnownRemoteSlotDataOpt == Option(
                BestChain(NonEmptyChain.one(allIdSlotDataHeaderBlock.last._2))
              )
            )
          } yield ()
        }
    }
  }

  test("RemoteBlockBodies: Verify and save only first block because of error in validation") {
    withMock {
      val reputationAggregator = mock[ReputationAggregatorActor[F]]
      val requestsProxy = mock[RequestsProxyActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val headerStore = mock[Store[F, BlockId, BlockHeader]]
      val bodyStore = mock[Store[F, BlockId, BlockBody]]
      val headerValidation = mock[BlockHeaderValidationAlgebra[F]]
      val bodySyntaxValidation = mock[BodySyntaxValidationAlgebra[F]]
      val bodySemanticValidation = mock[BodySemanticValidationAlgebra[F]]
      val bodyAuthorizationValidation = mock[BodyAuthorizationValidationAlgebra[F]]
      val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, SlotData]]

      // [1 - 5) -- known SlotData known Header known Body
      // [5 - 10) -- known SlotData known Header unknown Body
      // [10 - 15) -- known SlotData unknown Header unknown Body
      // Minimum length for allIdSlotDataHeaderBlock is 11 then
      val knownSlotHeaderBodyLen = 5
      val knownSlotHeaderUnknownBodyLen = 10
      val downloadedBodies = 10
      val knownSlotUnknownHeaderBodyLen = 15
      val allIdSlotDataHeaderBlock =
        arbitraryLinkedSlotDataHeaderBlockNoTx(
          Gen.choose(knownSlotUnknownHeaderBodyLen, knownSlotUnknownHeaderBodyLen)
        ).arbitrary.first.toList

      val knowSlotData =
        allIdSlotDataHeaderBlock.map(data => (data._1, data._2))

      val knownHeaders =
        allIdSlotDataHeaderBlock.take(knownSlotHeaderUnknownBodyLen).map(d => (d._1, d._3))

      val knownBody =
        allIdSlotDataHeaderBlock.take(knownSlotHeaderBodyLen).map(d => (d._1, d._4))

      val requestIdSlotDataHeaderBlock =
        allIdSlotDataHeaderBlock.slice(knownSlotHeaderBodyLen, downloadedBodies)

      val messageData =
        NonEmptyChain.fromSeq(requestIdSlotDataHeaderBlock.map(d => Block(d._3, d._4))).get

      val message =
        BlockChecker.Message.RemoteBlockBodies(messageData.map(d => (d.header, UnverifiedBlockBody(hostId, d.body))))

      val knownBodyStorageData: mutable.Map[BlockId, BlockBody] =
        mutable.Map.empty[BlockId, BlockBody] ++ knownBody.toMap
      (bodyStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
        knownBodyStorageData.contains(id).pure[F]
      }
      (bodyStore.put(_: BlockId, _: BlockBody)).expects(*, *).anyNumberOfTimes().onCall {
        case (id: BlockId, block: BlockBody) =>
          knownBodyStorageData.put(id, block).pure[F].void
      }

      val headerStorageData = knownHeaders.toMap
      (headerStore
        .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
          headerStorageData(id).pure[F]
        }
      (headerStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
        headerStorageData.contains(id).pure[F]
      }
      (headerStore.get _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
        headerStorageData.get(id).pure[F]
      }

      (bodySyntaxValidation.validate _).expects(*).rep(2).onCall { b: BlockBody =>
        Validated.validNec[BodySyntaxError, BlockBody](b).pure[F]
      }

      val errorTransaction = arbitraryIoTransaction.arbitrary.first
      val spentTransactionOutput = arbitrarySpentTransactionOutput.arbitrary.first
      (bodySemanticValidation
        .validate(_: BodyValidationContext)(_: BlockBody))
        .expects(*, *)
        .rep(2)
        .onCall { case (context: BodyValidationContext, b: BlockBody) =>
          val secondBlock: Boolean = context.parentHeaderId != requestIdSlotDataHeaderBlock.head._1
          val error =
            TransactionSemanticErrors(errorTransaction, NonEmptyChain.one(InputDataMismatch(spentTransactionOutput)))
          Validated.condNec[BodySemanticError, BlockBody](secondBlock, b, error).pure[F]
        }

      type AuthContext = IoTransaction => DynamicContext[F, String, Datum]
      (bodyAuthorizationValidation
        .validate(_: AuthContext)(_: BlockBody))
        .expects(*, *)
        .rep(1)
        .onCall { case (_: AuthContext @unchecked, b: BlockBody) =>
          Validated.validNec[BodyAuthorizationError, BlockBody](b).pure[F]
        }

      val slotDataStoreData = knowSlotData.toMap
      (slotDataStore.get _).expects(*).anyNumberOfTimes().onCall { id: BlockId => slotDataStoreData.get(id).pure[F] }
      (slotDataStore
        .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
          slotDataStoreData(id).pure[F]
        }

      val lastAdoptedBlockSlotData = requestIdSlotDataHeaderBlock.head._2
      (localChain.isWorseThan _).expects(lastAdoptedBlockSlotData).once().returning(true.pure[F])
      (localChain.adopt _).expects(Validated.Valid(lastAdoptedBlockSlotData)).once().returning(().pure[F])

      (requestsProxy.sendNoWait _)
        .expects(RequestsProxy.Message.InvalidateBlockId(hostId, requestIdSlotDataHeaderBlock(1)._1))
        .returns(().pure[F])
      (requestsProxy.sendNoWait _)
        .expects(RequestsProxy.Message.GetCurrentTips)
        .returns(().pure[F])

      BlockChecker
        .makeActor(
          reputationAggregator,
          requestsProxy,
          localChain,
          slotDataStore,
          headerStore,
          bodyStore,
          headerValidation,
          bodySyntaxValidation,
          bodySemanticValidation,
          bodyAuthorizationValidation,
          chainSelectionAlgebra,
          Option(BestChain(NonEmptyChain.fromSeq(allIdSlotDataHeaderBlock.map(_._2)).get)),
          Option(hostId)
        )
        .use { actor =>
          for {
            _ <- actor.send(message)
          } yield ()
        }
    }
  }

  test("RemoteBlockBodies: Invalidate block on current best chain do clear best chain and host") {
    withMock {
      val reputationAggregator = mock[ReputationAggregatorActor[F]]
      val requestsProxy = mock[RequestsProxyActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val headerStore = mock[Store[F, BlockId, BlockHeader]]
      val bodyStore = mock[Store[F, BlockId, BlockBody]]
      val headerValidation = mock[BlockHeaderValidationAlgebra[F]]
      val bodySyntaxValidation = mock[BodySyntaxValidationAlgebra[F]]
      val bodySemanticValidation = mock[BodySemanticValidationAlgebra[F]]
      val bodyAuthorizationValidation = mock[BodyAuthorizationValidationAlgebra[F]]
      val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, SlotData]]

      val currentBestChain = arbitraryLinkedSlotDataChain.arbitrary.first
      val invalidSlotData = currentBestChain.get(Gen.choose(0, currentBestChain.size - 1).first).get

      (requestsProxy.sendNoWait _).expects(RequestsProxy.Message.GetCurrentTips).returns(().pure[F])

      BlockChecker
        .makeActor(
          reputationAggregator,
          requestsProxy,
          localChain,
          slotDataStore,
          headerStore,
          bodyStore,
          headerValidation,
          bodySyntaxValidation,
          bodySemanticValidation,
          bodyAuthorizationValidation,
          chainSelectionAlgebra,
          Option(BestChain(currentBestChain)),
          Option(hostId)
        )
        .use { actor =>
          for {
            state <- actor.send(BlockChecker.Message.InvalidateBlockId(invalidSlotData.slotId.blockId))
            _ = assert(state.bestKnownRemoteSlotDataOpt.isEmpty)
            _ = assert(state.bestKnownRemoteSlotDataHost.isEmpty)
          } yield ()
        }
    }
  }

  test("RemoteBlockBodies: Invalidate block on not current best chain do nothing") {
    withMock {
      val reputationAggregator = mock[ReputationAggregatorActor[F]]
      val requestsProxy = mock[RequestsProxyActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val headerStore = mock[Store[F, BlockId, BlockHeader]]
      val bodyStore = mock[Store[F, BlockId, BlockBody]]
      val headerValidation = mock[BlockHeaderValidationAlgebra[F]]
      val bodySyntaxValidation = mock[BodySyntaxValidationAlgebra[F]]
      val bodySemanticValidation = mock[BodySemanticValidationAlgebra[F]]
      val bodyAuthorizationValidation = mock[BodyAuthorizationValidationAlgebra[F]]
      val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, SlotData]]

      val currentBestChain = arbitraryLinkedSlotDataChain.arbitrary.first
      val invalidBlockId = arbitrarySlotData.arbitrary.first.slotId.blockId

      (requestsProxy.sendNoWait _).expects(RequestsProxy.Message.GetCurrentTips).never()

      BlockChecker
        .makeActor(
          reputationAggregator,
          requestsProxy,
          localChain,
          slotDataStore,
          headerStore,
          bodyStore,
          headerValidation,
          bodySyntaxValidation,
          bodySemanticValidation,
          bodyAuthorizationValidation,
          chainSelectionAlgebra,
          Option(BestChain(currentBestChain)),
          Option(hostId)
        )
        .use { actor =>
          for {
            state <- actor.send(BlockChecker.Message.InvalidateBlockId(invalidBlockId))
            _ = assert(state.bestKnownRemoteSlotDataOpt.isDefined)
            _ = assert(state.bestKnownRemoteSlotDataHost.isDefined)
          } yield ()
        }
    }
  }
}
