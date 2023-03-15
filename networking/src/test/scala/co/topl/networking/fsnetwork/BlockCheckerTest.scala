package co.topl.networking.fsnetwork

import cats.data._
import cats.effect.IO
import cats.implicits._
import cats.{MonadThrow, Show}
import co.topl.algebras.Store
import co.topl.brambl.models.Datum
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.codecs.bytes.tetra.instances._
import co.topl.consensus.algebras._
import co.topl.consensus.models._
import co.topl.ledger.algebras._
import co.topl.ledger.models.{BodyAuthorizationError, BodySemanticError, BodySyntaxError, BodyValidationContext}
import co.topl.models.generators.consensus.ModelGenerators._
import co.topl.models.generators.node.ModelGenerators
import co.topl.networking.fsnetwork.BlockCheckerTest.F
import co.topl.networking.fsnetwork.PeersManager.PeersManagerActor
import co.topl.networking.fsnetwork.ReputationAggregator.ReputationAggregatorActor
import co.topl.networking.fsnetwork.TestImplicits._
import co.topl.node.models.{Block, BlockBody}
import co.topl.quivr.runtime.DynamicContext
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.Gen
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.collection.mutable

object BlockCheckerTest {
  type F[A] = IO[A]
}

class BlockCheckerTest extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {
  implicit val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)
  val hostId: HostId = "127.0.0.1"
  val maxChainSize = 99

  test("RemoteSlotData: Request no headers if new slot data is worse") {
    withMock {
      val slotData: NonEmptyChain[SlotData] =
        arbitrarySlotDataChain.arbitrary.retryUntil(c => c.size < maxChainSize).sample.get

      val reputationAggregator = mock[ReputationAggregatorActor[F]]

      val peersManager = mock[PeersManagerActor[F]]

      val localChain = mock[LocalChainAlgebra[F]]
      val currentSlotDataHead = arbitrarySlotData.arbitrary.sample.get
      (localChain.head _).expects().once().onCall(() => currentSlotDataHead.pure[F])

      val slotDataStore = mock[Store[F, BlockId, SlotData]]

      val headerStore = mock[Store[F, BlockId, BlockHeader]]

      val bodyStore = mock[Store[F, BlockId, BlockBody]]

      val headerValidation = mock[BlockHeaderValidationAlgebra[F]]

      val headerToBodyValidation = mock[BlockHeaderToBodyValidationAlgebra[F]]

      val bodySyntaxValidation = mock[BodySyntaxValidationAlgebra[F]]

      val bodySemanticValidation = mock[BodySemanticValidationAlgebra[F]]

      val bodyAuthorizationValidation = mock[BodyAuthorizationValidationAlgebra[F]]

      val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, SlotData]]
      (chainSelectionAlgebra.compare _).expects(slotData.last, currentSlotDataHead).onCall { case (_, _) =>
        (-1).pure[F]
      }

      for {
        (actor, shutdown) <-
          BlockChecker
            .makeActor(
              reputationAggregator,
              peersManager,
              localChain,
              slotDataStore,
              headerStore,
              bodyStore,
              headerValidation,
              headerToBodyValidation,
              bodySyntaxValidation,
              bodySemanticValidation,
              bodyAuthorizationValidation,
              chainSelectionAlgebra
            )
            .allocated
        _             <- actor.sendNoWait(BlockChecker.Message.RemoteSlotData(hostId, slotData))
        shutdownFiber <- actor.gracefulShutdown(shutdown)
        _             <- shutdownFiber.join
      } yield ()
    }
  }

  test("RemoteSlotData: Request no headers if new slot data is better and is extension of current chain") {
    withMock {
      val reputationAggregator = mock[ReputationAggregatorActor[F]]
      val peersManager = mock[PeersManagerActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val headerStore = mock[Store[F, BlockId, BlockHeader]]
      val bodyStore = mock[Store[F, BlockId, BlockBody]]
      val headerValidation = mock[BlockHeaderValidationAlgebra[F]]
      val headerToBodyValidation = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val bodySyntaxValidation = mock[BodySyntaxValidationAlgebra[F]]
      val bodySemanticValidation = mock[BodySemanticValidationAlgebra[F]]
      val bodyAuthorizationValidation = mock[BodyAuthorizationValidationAlgebra[F]]
      val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, SlotData]]

      val slotData: NonEmptyChain[SlotData] =
        arbitrarySlotDataChain.arbitrary.retryUntil(c => c.size > 2 && c.size < maxChainSize).sample.get
      val idAndSlotData: NonEmptyChain[(BlockId, SlotData)] = slotData.map(s => (s.slotId.blockId, s))

      // local is known data which are stored in stores
      val (localId, localSlotData) = idAndSlotData.head
      (slotDataStore
        .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
        .expects(localId, *, *)
        .once()
        .onCall { case (_: BlockId, _: MonadThrow[F], _: Show[BlockId]) => localSlotData.pure[F] }
      (headerStore.contains _).expects(localId).once().onCall { _: BlockId => true.pure[F] }

      // slot data from peer
      val remoteSlotDataAndId = NonEmptyChain.fromChain(idAndSlotData.tail).get
      val (_, remoteSlotData) = remoteSlotDataAndId.unzip

      (chainSelectionAlgebra.compare _).expects(slotData.last, localSlotData).onCall { case (_, _) =>
        1.pure[F]
      }

      for {
        (actor, shutdown) <-
          BlockChecker
            .makeActor(
              reputationAggregator,
              peersManager,
              localChain,
              slotDataStore,
              headerStore,
              bodyStore,
              headerValidation,
              headerToBodyValidation,
              bodySyntaxValidation,
              bodySemanticValidation,
              bodyAuthorizationValidation,
              chainSelectionAlgebra,
              Option(BestChain(NonEmptyChain.one(localSlotData)))
            )
            .allocated
        updatedState <- actor.send(BlockChecker.Message.RemoteSlotData(hostId, remoteSlotData))
        _ = assert(updatedState.bestKnownRemoteSlotDataOpt == Option(BestChain(remoteSlotData)))
        shutdownFiber <- actor.gracefulShutdown(shutdown)
        _             <- shutdownFiber.join
      } yield ()
    }
  }

  test("RemoteSlotData: Request no headers if new slot data is better and is overlapped of current chain") {
    withMock {
      val reputationAggregator = mock[ReputationAggregatorActor[F]]
      val peersManager = mock[PeersManagerActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val headerStore = mock[Store[F, BlockId, BlockHeader]]
      val bodyStore = mock[Store[F, BlockId, BlockBody]]
      val headerValidation = mock[BlockHeaderValidationAlgebra[F]]
      val headerToBodyValidation = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val bodySyntaxValidation = mock[BodySyntaxValidationAlgebra[F]]
      val bodySemanticValidation = mock[BodySemanticValidationAlgebra[F]]
      val bodyAuthorizationValidation = mock[BodyAuthorizationValidationAlgebra[F]]
      val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, SlotData]]

      val slotData: NonEmptyChain[SlotData] =
        arbitrarySlotDataChain.arbitrary.retryUntil(c => c.size > 2 && c.size < maxChainSize).sample.get
      val idAndSlotData: NonEmptyChain[(BlockId, SlotData)] = slotData.map(s => (s.slotId.blockId, s))

      // local is known data which are stored in stores
      val (localId, localSlotData) = idAndSlotData.head
      (slotDataStore
        .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
        .expects(localId, *, *)
        .once()
        .onCall { case (_: BlockId, _: MonadThrow[F], _: Show[BlockId]) => localSlotData.pure[F] }
      (headerStore.contains _).expects(localId).once().onCall { _: BlockId => true.pure[F] }

      // slot data from peer
      val remoteSlotDataAndId = NonEmptyChain.fromChain(idAndSlotData.tail).get
      val (_, remoteSlotData) = remoteSlotDataAndId.unzip

      // known but not accepted yet data
      val knownSlotData = NonEmptyChain.fromSeq(slotData.toChain.toList.take(2)).get

      (chainSelectionAlgebra.compare _).expects(slotData.last, knownSlotData.last).onCall { case (_, _) =>
        1.pure[F]
      }

      for {
        (actor, shutdown) <-
          BlockChecker
            .makeActor(
              reputationAggregator,
              peersManager,
              localChain,
              slotDataStore,
              headerStore,
              bodyStore,
              headerValidation,
              headerToBodyValidation,
              bodySyntaxValidation,
              bodySemanticValidation,
              bodyAuthorizationValidation,
              chainSelectionAlgebra,
              Option(BestChain(NonEmptyChain.one(remoteSlotData.head)))
            )
            .allocated
        updatedState <- actor.send(BlockChecker.Message.RemoteSlotData(hostId, remoteSlotData))
        _ = assert(updatedState.bestKnownRemoteSlotDataOpt == Option(BestChain(remoteSlotData)))
        shutdownFiber <- actor.gracefulShutdown(shutdown)
        _             <- shutdownFiber.join
      } yield ()
    }
  }

  test("RemoteSlotData: Request headers if new slot data is better and is not extension of current chain") {
    withMock {
      val reputationAggregator = mock[ReputationAggregatorActor[F]]
      val peersManager = mock[PeersManagerActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val headerStore = mock[Store[F, BlockId, BlockHeader]]
      val bodyStore = mock[Store[F, BlockId, BlockBody]]
      val headerValidation = mock[BlockHeaderValidationAlgebra[F]]
      val headerToBodyValidation = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val bodySyntaxValidation = mock[BodySyntaxValidationAlgebra[F]]
      val bodySemanticValidation = mock[BodySemanticValidationAlgebra[F]]
      val bodyAuthorizationValidation = mock[BodyAuthorizationValidationAlgebra[F]]
      val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, SlotData]]

      val slotData: NonEmptyChain[SlotData] =
        arbitrarySlotDataChain.arbitrary.retryUntil(c => c.size > 2 && c.size < maxChainSize).sample.get
      val idAndSlotData: NonEmptyChain[(BlockId, SlotData)] = slotData.map(s => (s.slotId.blockId, s))

      // local is known data which are stored in stores
      val (localId, localSlotData) = idAndSlotData.head
      (slotDataStore
        .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
        .expects(localId, *, *)
        .once()
        .onCall { case (_: BlockId, _: MonadThrow[F], _: Show[BlockId]) => localSlotData.pure[F] }

      // slot data from peer
      val remoteSlotDataAndId = NonEmptyChain.fromChain(idAndSlotData.tail).get
      val (_, remoteSlotData) = remoteSlotDataAndId.unzip

      (localChain.head _).expects().once().onCall(() => localSlotData.pure[F])

      (chainSelectionAlgebra.compare _).expects(slotData.last, localSlotData).onCall { case (_, _) =>
        1.pure[F]
      }

      val localSlotDataStore = idAndSlotData.toList.toMap
      (slotDataStore
        .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, _: MonadThrow[F], _: Show[BlockId]) =>
          localSlotDataStore(id).pure[F]
        }

      (headerStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId => (id == localId).pure[F] }

      val expectedRequest =
        PeersManager.Message.BlockHeadersRequest(
          hostId,
          NonEmptyChain.fromSeq(remoteSlotDataAndId.toChain.toList.take(chunkSize).map(_._1)).get
        ): PeersManager.Message
      (peersManager.sendNoWait _).expects(expectedRequest).once().onCall { _: PeersManager.Message => ().pure[F] }

      val expectedBestChain = Option(BestChain(NonEmptyChain.fromChain(slotData.tail).get))

      for {
        (actor, shutdown) <-
          BlockChecker
            .makeActor(
              reputationAggregator,
              peersManager,
              localChain,
              slotDataStore,
              headerStore,
              bodyStore,
              headerValidation,
              headerToBodyValidation,
              bodySyntaxValidation,
              bodySemanticValidation,
              bodyAuthorizationValidation,
              chainSelectionAlgebra
            )
            .allocated
        updatedState <- actor.send(BlockChecker.Message.RemoteSlotData(hostId, remoteSlotData))
        _ = assert(updatedState.bestKnownRemoteSlotDataOpt == expectedBestChain)
        shutdownFiber <- actor.gracefulShutdown(shutdown)
        _             <- shutdownFiber.join
      } yield ()
    }
  }

  test("RemoteSlotData: Request headers if new slot data is better, building full chain") {
    withMock {
      val reputationAggregator = mock[ReputationAggregatorActor[F]]
      val peersManager = mock[PeersManagerActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val headerStore = mock[Store[F, BlockId, BlockHeader]]
      val bodyStore = mock[Store[F, BlockId, BlockBody]]
      val headerValidation = mock[BlockHeaderValidationAlgebra[F]]
      val headerToBodyValidation = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val bodySyntaxValidation = mock[BodySyntaxValidationAlgebra[F]]
      val bodySemanticValidation = mock[BodySemanticValidationAlgebra[F]]
      val bodyAuthorizationValidation = mock[BodyAuthorizationValidationAlgebra[F]]
      val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, SlotData]]

      val slotData: NonEmptyChain[SlotData] =
        arbitrarySlotDataChain.arbitrary.retryUntil(c => c.size > 3 && c.size < maxChainSize).sample.get
      val idAndSlotData: NonEmptyChain[(BlockId, SlotData)] = slotData.map(s => (s.slotId.blockId, s))

      // local is known data which are stored in stores
      val (localId, _) = idAndSlotData.head
      (headerStore.contains _).expects(localId).once().onCall { _: BlockId => true.pure[F] }

      // slot data from peer
      val remoteSize = Gen.choose[Int](2, idAndSlotData.tail.size.toInt - 1).sample.get
      val (missedInChainData, remoteIdAndSlotData) = idAndSlotData.tail.toList.splitAt(remoteSize)
      val (_, peerSlotData) = remoteIdAndSlotData.unzip

      val localSlotDataStore = idAndSlotData.toList.toMap
      (slotDataStore
        .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, _: MonadThrow[F], _: Show[BlockId]) =>
          localSlotDataStore(id).pure[F]
        }

      (headerStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId => (id == localId).pure[F] }

      val currentBestChain = NonEmptyChain.fromSeq(slotData.toList.take(2)).get
      (chainSelectionAlgebra.compare _).expects(slotData.last, currentBestChain.last).onCall { case (_, _) =>
        1.pure[F]
      }

      val expectedRequest =
        PeersManager.Message.BlockHeadersRequest(
          hostId,
          NonEmptyChain.fromSeq(missedInChainData.take(chunkSize).map(_._1)).get
        ): PeersManager.Message
      (peersManager.sendNoWait _).expects(expectedRequest).once().onCall { _: PeersManager.Message => ().pure[F] }

      val expectedBestChain = Option(BestChain(NonEmptyChain.fromChain(slotData.tail).get))

      for {
        (actor, shutdown) <-
          BlockChecker
            .makeActor(
              reputationAggregator,
              peersManager,
              localChain,
              slotDataStore,
              headerStore,
              bodyStore,
              headerValidation,
              headerToBodyValidation,
              bodySyntaxValidation,
              bodySemanticValidation,
              bodyAuthorizationValidation,
              chainSelectionAlgebra,
              Option(BestChain(currentBestChain))
            )
            .allocated
        updatedState <- actor.send(BlockChecker.Message.RemoteSlotData(hostId, NonEmptyChain.fromSeq(peerSlotData).get))
        _ = assert(updatedState.bestKnownRemoteSlotDataOpt == expectedBestChain)
        shutdownFiber <- actor.gracefulShutdown(shutdown)
        _             <- shutdownFiber.join
      } yield ()
    }
  }

  test("RemoteBlockHeader: Do not verify already known headers") {
    withMock {
      val headers: NonEmptyChain[BlockHeader] =
        nonEmptyChainArbOf(arbitraryHeader).arbitrary.retryUntil(c => c.size > 0 && c.size < maxChainSize).sample.get
      val idAndHeaders = headers.map(h => (h.id, h))

      val reputationAggregator = mock[ReputationAggregatorActor[F]]
      val peersManager = mock[PeersManagerActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val headerStore = mock[Store[F, BlockId, BlockHeader]]
      val bodyStore = mock[Store[F, BlockId, BlockBody]]
      val headerValidation = mock[BlockHeaderValidationAlgebra[F]]
      val headerToBodyValidation = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val bodySyntaxValidation = mock[BodySyntaxValidationAlgebra[F]]
      val bodySemanticValidation = mock[BodySemanticValidationAlgebra[F]]
      val bodyAuthorizationValidation = mock[BodyAuthorizationValidationAlgebra[F]]
      val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, SlotData]]

      (headerStore.contains _).expects(*).rep(headers.size.toInt).onCall { _: BlockId => true.pure[F] }
      (headerValidation.validate _).expects(*, *).never()

      for {
        (actor, shutdown) <-
          BlockChecker
            .makeActor(
              reputationAggregator,
              peersManager,
              localChain,
              slotDataStore,
              headerStore,
              bodyStore,
              headerValidation,
              headerToBodyValidation,
              bodySyntaxValidation,
              bodySemanticValidation,
              bodyAuthorizationValidation,
              chainSelectionAlgebra
            )
            .allocated
        _             <- actor.sendNoWait(BlockChecker.Message.RemoteBlockHeader(hostId, idAndHeaders))
        shutdownFiber <- actor.gracefulShutdown(shutdown)
        _             <- shutdownFiber.join
      } yield ()
    }
  }

  test("RemoteBlockHeader: Do not verify already known headers, stop processing if all headers are known") {
    withMock {
      val headers: NonEmptyChain[BlockHeader] =
        arbitraryBlockHeaderChain(Gen.choose(2, maxChainSize)).arbitrary.sample.get
      val idAndHeaders = headers.map(h => (h.id, h))

      val reputationAggregator = mock[ReputationAggregatorActor[F]]
      val peersManager = mock[PeersManagerActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val headerStore = mock[Store[F, BlockId, BlockHeader]]
      val bodyStore = mock[Store[F, BlockId, BlockBody]]
      val headerValidation = mock[BlockHeaderValidationAlgebra[F]]
      val headerToBodyValidation = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val bodySyntaxValidation = mock[BodySyntaxValidationAlgebra[F]]
      val bodySemanticValidation = mock[BodySemanticValidationAlgebra[F]]
      val bodyAuthorizationValidation = mock[BodyAuthorizationValidationAlgebra[F]]
      val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, SlotData]]

      (headerStore.contains _).expects(*).rep(headers.size.toInt).onCall { _: BlockId => true.pure[F] }
      (headerValidation.validate _).expects(*, *).never()

      for {
        (actor, shutdown) <-
          BlockChecker
            .makeActor(
              reputationAggregator,
              peersManager,
              localChain,
              slotDataStore,
              headerStore,
              bodyStore,
              headerValidation,
              headerToBodyValidation,
              bodySyntaxValidation,
              bodySemanticValidation,
              bodyAuthorizationValidation,
              chainSelectionAlgebra
            )
            .allocated
        _             <- actor.sendNoWait(BlockChecker.Message.RemoteBlockHeader(hostId, idAndHeaders))
        shutdownFiber <- actor.gracefulShutdown(shutdown)
        _             <- shutdownFiber.join
      } yield ()
    }
  }

  test("RemoteBlockHeader: Do not verify already known headers, request bodies, do not request next headers") {
    withMock {
      val reputationAggregator = mock[ReputationAggregatorActor[F]]
      val peersManager = mock[PeersManagerActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val headerStore = mock[Store[F, BlockId, BlockHeader]]
      val bodyStore = mock[Store[F, BlockId, BlockBody]]
      val headerValidation = mock[BlockHeaderValidationAlgebra[F]]
      val headerToBodyValidation = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val bodySyntaxValidation = mock[BodySyntaxValidationAlgebra[F]]
      val bodySemanticValidation = mock[BodySemanticValidationAlgebra[F]]
      val bodyAuthorizationValidation = mock[BodyAuthorizationValidationAlgebra[F]]
      val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, SlotData]]

      val headers: NonEmptyChain[BlockHeader] =
        arbitraryBlockHeaderChain(Gen.choose(2, maxChainSize)).arbitrary.sample.get
      val idAndHeaders = headers.map(h => (h.id, h))

      val knownHeadersSize = Gen.choose[Int](1, headers.size.toInt - 1).sample.get
      val (knownIdAndHeaders, newIdAndHeaders) = idAndHeaders.toList.splitAt(knownHeadersSize)

      val headerStoreData = mutable.Map.empty[BlockId, BlockHeader] ++ knownIdAndHeaders.toMap
      (headerStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
        headerStoreData.contains(id).pure[F]
      }
      (headerStore
        .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, _: MonadThrow[F], _: Show[BlockId]) =>
          headerStoreData(id).pure[F]
        }
      val addedHeader = mutable.Map.empty[BlockId, BlockHeader]
      (headerStore.put _).expects(*, *).rep(newIdAndHeaders.size).onCall { case (id: BlockId, header: BlockHeader) =>
        addedHeader.put(id, header)
        headerStoreData.put(id, header)
        ().pure[F]
      }

      (headerValidation.validate _).expects(*, *).rep(newIdAndHeaders.size).onCall {
        case (header: BlockHeader, _: BlockHeader) =>
          Either.right[BlockHeaderValidationFailure, BlockHeader](header).pure[F]
      }

      val expectedIds = NonEmptyChain.fromSeq(newIdAndHeaders.map(_._1)).get
      val expectedMessage: PeersManager.Message = PeersManager.Message.BlockDownloadRequest(hostId, expectedIds)
      (peersManager.sendNoWait _).expects(expectedMessage).onCall { _: PeersManager.Message => ().pure[F] }

      (peersManager.sendNoWait _).expects(*).never() // no other requests

      for {
        (actor, shutdown) <-
          BlockChecker
            .makeActor(
              reputationAggregator,
              peersManager,
              localChain,
              slotDataStore,
              headerStore,
              bodyStore,
              headerValidation,
              headerToBodyValidation,
              bodySyntaxValidation,
              bodySemanticValidation,
              bodyAuthorizationValidation,
              chainSelectionAlgebra
            )
            .allocated
        _ <- actor.send(BlockChecker.Message.RemoteBlockHeader(hostId, idAndHeaders))
        _ = assert(newIdAndHeaders.map(_._1).forall(k => addedHeader.contains(k)))
        shutdownFiber <- actor.gracefulShutdown(shutdown)
        _             <- shutdownFiber.join
      } yield ()
    }
  }

  test("RemoteBlockHeader: Do not verify known headers, request bodies, request next headers because best chain") {
    withMock {
      val reputationAggregator = mock[ReputationAggregatorActor[F]]
      val peersManager = mock[PeersManagerActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val headerStore = mock[Store[F, BlockId, BlockHeader]]
      val bodyStore = mock[Store[F, BlockId, BlockBody]]
      val headerValidation = mock[BlockHeaderValidationAlgebra[F]]
      val headerToBodyValidation = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val bodySyntaxValidation = mock[BodySyntaxValidationAlgebra[F]]
      val bodySemanticValidation = mock[BodySemanticValidationAlgebra[F]]
      val bodyAuthorizationValidation = mock[BodyAuthorizationValidationAlgebra[F]]
      val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, SlotData]]

      val headers: NonEmptyChain[BlockHeader] =
        arbitraryBlockHeaderChain(Gen.choose(2, maxChainSize)).arbitrary.sample.get
      val idAndHeaders = headers.map(h => (h.id, h))

      val knownHeadersSize = Gen.choose[Int](1, headers.size.toInt - 1).sample.get
      val (knownIdAndHeaders, newIdAndHeaders) = idAndHeaders.toList.splitAt(knownHeadersSize)

      val headerStoreData = mutable.Map.empty[BlockId, BlockHeader] ++ knownIdAndHeaders.toMap
      (headerStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId =>
        headerStoreData.contains(id).pure[F]
      }
      (headerStore
        .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, _: MonadThrow[F], _: Show[BlockId]) =>
          headerStoreData(id).pure[F]
        }
      val addedHeader = mutable.Map.empty[BlockId, BlockHeader]
      (headerStore.put _).expects(*, *).rep(newIdAndHeaders.size).onCall { case (id: BlockId, header: BlockHeader) =>
        addedHeader.put(id, header)
        headerStoreData.put(id, header)
        ().pure[F]
      }

      (headerValidation.validate _).expects(*, *).rep(newIdAndHeaders.size).onCall {
        case (header: BlockHeader, _: BlockHeader) =>
          Either.right[BlockHeaderValidationFailure, BlockHeader](header).pure[F]
      }

      val expectedIds = NonEmptyChain.fromSeq(newIdAndHeaders.map(_._1)).get
      val expectedMessage: PeersManager.Message = PeersManager.Message.BlockDownloadRequest(hostId, expectedIds)
      (peersManager.sendNoWait _).expects(expectedMessage).onCall { _: PeersManager.Message => ().pure[F] }

      val bestChainForKnownAndNewIds: NonEmptyChain[SlotData] =
        idAndHeaders.map { case (id, header) =>
          val parentId = header.parentHeaderId
          val slotId = SlotId(blockId = id)
          val parentSlotId = SlotId(blockId = parentId)
          arbitrarySlotData.arbitrary.sample.get.copy(slotId = slotId, parentSlotId = parentSlotId)
        }

      val newSlotData: Chain[SlotData] = {
        val arSlot = arbitrarySlotDataChain.arbitrary.retryUntil(c => c.size > 1 && c.size < 10).sample.get
        arSlot.head.copy(parentSlotId = bestChainForKnownAndNewIds.last.slotId) +: arSlot.tail
      }

      val bestChain = bestChainForKnownAndNewIds.appendChain(newSlotData)

      val idAndSlotData = bestChain.map(s => (s.slotId.blockId, s))
      val localSlotDataStore = idAndSlotData.toList.toMap
      (slotDataStore
        .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (id: BlockId, _: MonadThrow[F], _: Show[BlockId]) =>
          localSlotDataStore(id).pure[F]
        }

      val nextHeaders = newSlotData.toList.take(chunkSize).map(s => s.slotId.blockId)

      val nextHeaderMessage: PeersManager.Message =
        PeersManager.Message.BlockHeadersRequest(hostId, NonEmptyChain.fromSeq(nextHeaders).get)
      (peersManager.sendNoWait _).expects(nextHeaderMessage).once().onCall { _: PeersManager.Message => ().pure[F] }

      for {
        (actor, shutdown) <-
          BlockChecker
            .makeActor(
              reputationAggregator,
              peersManager,
              localChain,
              slotDataStore,
              headerStore,
              bodyStore,
              headerValidation,
              headerToBodyValidation,
              bodySyntaxValidation,
              bodySemanticValidation,
              bodyAuthorizationValidation,
              chainSelectionAlgebra,
              Option(BestChain(bestChain)),
              Option(hostId)
            )
            .allocated
        _ <- actor.send(BlockChecker.Message.RemoteBlockHeader(hostId, idAndHeaders))
        _ = assert(newIdAndHeaders.map(_._1).forall(k => addedHeader.contains(k)))
        shutdownFiber <- actor.gracefulShutdown(shutdown)
        _             <- shutdownFiber.join
      } yield ()
    }
  }

  test("RemoteBlockBodies: Skip verification already known bodies") {
    withMock {
      val reputationAggregator = mock[ReputationAggregatorActor[F]]
      val peersManager = mock[PeersManagerActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val headerStore = mock[Store[F, BlockId, BlockHeader]]
      val bodyStore = mock[Store[F, BlockId, BlockBody]]
      val headerValidation = mock[BlockHeaderValidationAlgebra[F]]
      val headerToBodyValidation = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val bodySyntaxValidation = mock[BodySyntaxValidationAlgebra[F]]
      val bodySemanticValidation = mock[BodySemanticValidationAlgebra[F]]
      val bodyAuthorizationValidation = mock[BodyAuthorizationValidationAlgebra[F]]
      val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, SlotData]]

      val bodies =
        nonEmptyChainArbOf(ModelGenerators.arbitraryNodeBody).arbitrary
          .retryUntil(c => c.size > 1 && c.size < maxChainSize)
          .sample
          .get
      val ids: NonEmptyChain[BlockId] = bodies.map(_ => arbitraryHeader.arbitrary.sample.get.id)

      val idAndBody: NonEmptyChain[(BlockId, BlockBody)] = ids.zipWith(bodies)((_, _))

      val message = BlockChecker.Message.RemoteBlockBodies(hostId, idAndBody)

      (bodyStore.contains _).expects(*).rep(bodies.size.toInt).onCall { _: BlockId => true.pure[F] }
      (peersManager.sendNoWait _).expects(*).never()

      for {
        (actor, shutdown) <-
          BlockChecker
            .makeActor(
              reputationAggregator,
              peersManager,
              localChain,
              slotDataStore,
              headerStore,
              bodyStore,
              headerValidation,
              headerToBodyValidation,
              bodySyntaxValidation,
              bodySemanticValidation,
              bodyAuthorizationValidation,
              chainSelectionAlgebra
            )
            .allocated
        _             <- actor.send(message)
        shutdownFiber <- actor.gracefulShutdown(shutdown)
        _             <- shutdownFiber.join
      } yield ()
    }
  }

  test("RemoteBlockBodies: Verify and save new blocks, but not apply to local chain because it worse") {
    withMock {
      val reputationAggregator = mock[ReputationAggregatorActor[F]]
      val peersManager = mock[PeersManagerActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val headerStore = mock[Store[F, BlockId, BlockHeader]]
      val bodyStore = mock[Store[F, BlockId, BlockBody]]
      val headerValidation = mock[BlockHeaderValidationAlgebra[F]]
      val headerToBodyValidation = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val bodySyntaxValidation = mock[BodySyntaxValidationAlgebra[F]]
      val bodySemanticValidation = mock[BodySemanticValidationAlgebra[F]]
      val bodyAuthorizationValidation = mock[BodyAuthorizationValidationAlgebra[F]]
      val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, SlotData]]

      val bodies =
        nonEmptyChainArbOf(ModelGenerators.arbitraryNodeBody).arbitrary
          .retryUntil(c => c.size > 1 && c.size < maxChainSize)
          .sample
          .get
      val ids: NonEmptyChain[BlockId] = bodies.map(_ => arbitraryHeader.arbitrary.sample.get.id)

      val idAndBody: NonEmptyChain[(BlockId, BlockBody)] = ids.zipWith(bodies)((_, _))
      val idAndHeader: NonEmptyChain[(BlockId, BlockHeader)] =
        ids.map(id => (id, arbitraryHeader.arbitrary.sample.get))

      val message = BlockChecker.Message.RemoteBlockBodies(hostId, idAndBody)

      val knownBodiesSize = Gen.choose[Int](1, bodies.size.toInt - 1).sample.get
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
        .onCall { case (id: BlockId, _: MonadThrow[F], _: Show[BlockId]) =>
          headerStorageData(id).pure[F]
        }

      (headerToBodyValidation.validate _).expects(*).rep(newBodiesSize).onCall { b: Block =>
        Either.right[BlockHeaderToBodyValidationFailure, Block](b).pure[F]
      }

      (bodySyntaxValidation.validate _).expects(*).rep(newBodiesSize).onCall { b: BlockBody =>
        Validated.condNec[BodySyntaxError, BlockBody](test = true, b, null).pure[F]
      }

      (bodySemanticValidation
        .validate(_: BodyValidationContext)(_: BlockBody))
        .expects(*, *)
        .rep(newBodiesSize)
        .onCall { case (_: BodyValidationContext, b: BlockBody) =>
          Validated.condNec[BodySemanticError, BlockBody](test = true, b, null).pure[F]
        }

      type AuthContext = IoTransaction => DynamicContext[F, String, Datum]
      (bodyAuthorizationValidation
        .validate(_: AuthContext)(_: BlockBody))
        .expects(*, *)
        .rep(newBodiesSize)
        .onCall { case (_: AuthContext, b: BlockBody) =>
          Validated.condNec[BodyAuthorizationError, BlockBody](test = true, b, null).pure[F]
        }

      val storedBodies = mutable.Map.empty[BlockId, BlockBody]
      (bodyStore.put _).expects(*, *).rep(newBodiesSize).onCall { case (id: BlockId, block: BlockBody) =>
        storedBodies.put(id, block).pure[F].void
      }

      val lastBlockSlotData = arbitrarySlotData.arbitrary.sample.get

      val lastBlockId = idAndHeader.last._2.id // shall be ids.last but we generate headers
      (slotDataStore.get _).expects(lastBlockId).onCall { _: BlockId =>
        Option(lastBlockSlotData).pure[F]
      }

      (localChain.isWorseThan _).expects(lastBlockSlotData).once().onCall { _: SlotData => false.pure[F] }

      (peersManager.sendNoWait _).expects(*).never()

      for {
        (actor, shutdown) <-
          BlockChecker
            .makeActor(
              reputationAggregator,
              peersManager,
              localChain,
              slotDataStore,
              headerStore,
              bodyStore,
              headerValidation,
              headerToBodyValidation,
              bodySyntaxValidation,
              bodySemanticValidation,
              bodyAuthorizationValidation,
              chainSelectionAlgebra
            )
            .allocated
        _             <- actor.send(message)
        shutdownFiber <- actor.gracefulShutdown(shutdown)
        _             <- shutdownFiber.join
      } yield ()
    }
  }

  test("RemoteBlockBodies: Verify and save new blocks, and apply to local chain because it better") {
    withMock {
      val reputationAggregator = mock[ReputationAggregatorActor[F]]
      val peersManager = mock[PeersManagerActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val headerStore = mock[Store[F, BlockId, BlockHeader]]
      val bodyStore = mock[Store[F, BlockId, BlockBody]]
      val headerValidation = mock[BlockHeaderValidationAlgebra[F]]
      val headerToBodyValidation = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val bodySyntaxValidation = mock[BodySyntaxValidationAlgebra[F]]
      val bodySemanticValidation = mock[BodySemanticValidationAlgebra[F]]
      val bodyAuthorizationValidation = mock[BodyAuthorizationValidationAlgebra[F]]
      val chainSelectionAlgebra = mock[ChainSelectionAlgebra[F, SlotData]]

      val bodies =
        nonEmptyChainArbOf(ModelGenerators.arbitraryNodeBody).arbitrary
          .retryUntil(c => c.size > 1 && c.size < maxChainSize)
          .sample
          .get
      val ids: NonEmptyChain[BlockId] = bodies.map(_ => arbitraryHeader.arbitrary.sample.get.id)

      val idAndBody: NonEmptyChain[(BlockId, BlockBody)] = ids.zipWith(bodies)((_, _))
      val idAndHeader: NonEmptyChain[(BlockId, BlockHeader)] =
        ids.map(id => (id, arbitraryHeader.arbitrary.sample.get))

      val message = BlockChecker.Message.RemoteBlockBodies(hostId, idAndBody)

      val knownBodiesSize = Gen.choose[Int](1, bodies.size.toInt - 1).sample.get
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
        .onCall { case (id: BlockId, _: MonadThrow[F], _: Show[BlockId]) =>
          headerStorageData(id).pure[F]
        }

      (headerToBodyValidation.validate _).expects(*).rep(newBodiesSize).onCall { b: Block =>
        Either.right[BlockHeaderToBodyValidationFailure, Block](b).pure[F]
      }

      (bodySyntaxValidation.validate _).expects(*).rep(newBodiesSize).onCall { b: BlockBody =>
        Validated.condNec[BodySyntaxError, BlockBody](test = true, b, null).pure[F]
      }

      (bodySemanticValidation
        .validate(_: BodyValidationContext)(_: BlockBody))
        .expects(*, *)
        .rep(newBodiesSize)
        .onCall { case (_: BodyValidationContext, b: BlockBody) =>
          Validated.condNec[BodySemanticError, BlockBody](test = true, b, null).pure[F]
        }

      type AuthContext = IoTransaction => DynamicContext[F, String, Datum]
      (bodyAuthorizationValidation
        .validate(_: AuthContext)(_: BlockBody))
        .expects(*, *)
        .rep(newBodiesSize)
        .onCall { case (_: AuthContext, b: BlockBody) =>
          Validated.condNec[BodyAuthorizationError, BlockBody](test = true, b, null).pure[F]
        }

      val storedBodies = mutable.Map.empty[BlockId, BlockBody]
      (bodyStore.put _).expects(*, *).rep(newBodiesSize).onCall { case (id: BlockId, block: BlockBody) =>
        storedBodies.put(id, block).pure[F].void
      }

      val lastBlockSlotData = arbitrarySlotData.arbitrary.sample.get

      val lastBlockId = idAndHeader.last._2.id // shall be ids.last but we generate headers
      (slotDataStore.get _).expects(lastBlockId).onCall { _: BlockId =>
        Option(lastBlockSlotData).pure[F]
      }

      (localChain.isWorseThan _).expects(lastBlockSlotData).once().onCall { _: SlotData => true.pure[F] }
      (localChain.adopt _).expects(Validated.Valid(lastBlockSlotData)).once().onCall { _: Validated.Valid[SlotData] =>
        ().pure[F]
      }

      (peersManager.sendNoWait _).expects(*).never()

      for {
        (actor, shutdown) <-
          BlockChecker
            .makeActor(
              reputationAggregator,
              peersManager,
              localChain,
              slotDataStore,
              headerStore,
              bodyStore,
              headerValidation,
              headerToBodyValidation,
              bodySyntaxValidation,
              bodySemanticValidation,
              bodyAuthorizationValidation,
              chainSelectionAlgebra,
              Option(BestChain(NonEmptyChain.one(lastBlockSlotData)))
            )
            .allocated
        newState <- actor.send(message)
        _ = assert(newState.bestKnownRemoteSlotDataOpt.isEmpty)
        shutdownFiber <- actor.gracefulShutdown(shutdown)
        _             <- shutdownFiber.join
      } yield ()
    }
  }

}
