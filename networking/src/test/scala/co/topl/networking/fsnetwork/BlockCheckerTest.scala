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
import co.topl.models.ModelGenerators.GenHelper
import co.topl.models.generators.consensus.ModelGenerators._
import co.topl.models.generators.node.ModelGenerators
import co.topl.networking.fsnetwork.BlockCheckerTest.F
import co.topl.networking.fsnetwork.PeersManager.PeersManagerActor
import co.topl.networking.fsnetwork.ReputationAggregator.ReputationAggregatorActor
import co.topl.networking.fsnetwork.TestHelper._
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
        arbitraryLinkedSlotDataChain.arbitrary.retryUntil(c => c.size < maxChainSize).first

      val reputationAggregator = mock[ReputationAggregatorActor[F]]

      val peersManager = mock[PeersManagerActor[F]]

      val localChain = mock[LocalChainAlgebra[F]]
      val currentSlotDataHead = arbitrarySlotData.arbitrary.first
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

      (chainSelectionAlgebra.compare _).expects(slotData.last, currentSlotDataHead).returning((-1).pure[F])

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
        .use { actor =>
          for {
            _ <- actor.sendNoWait(BlockChecker.Message.RemoteSlotData(hostId, slotData))
          } yield ()
        }
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
        arbitraryLinkedSlotDataChain.arbitrary.retryUntil(c => c.size > 2 && c.size < maxChainSize).first
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
        .use { actor =>
          for {
            updatedState <- actor.send(BlockChecker.Message.RemoteSlotData(hostId, remoteSlotData))
            _ = assert(updatedState.bestKnownRemoteSlotDataOpt == Option(BestChain(remoteSlotData)))
          } yield ()
        }
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
        arbitraryLinkedSlotDataChain.arbitrary.retryUntil(c => c.size > 2 && c.size < maxChainSize).first
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
        .use { actor =>
          for {
            updatedState <- actor.send(BlockChecker.Message.RemoteSlotData(hostId, remoteSlotData))
            _ = assert(updatedState.bestKnownRemoteSlotDataOpt == Option(BestChain(remoteSlotData)))
          } yield ()
        }
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
        arbitraryLinkedSlotDataChain.arbitrary.retryUntil(c => c.size > 2 && c.size < maxChainSize).first
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

      (localChain.head _).expects().once().returning(localSlotData.pure[F])

      (chainSelectionAlgebra.compare _).expects(slotData.last, localSlotData).returning(1.pure[F])

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
      (peersManager.sendNoWait _).expects(expectedRequest).once().returning(().pure[F])

      val expectedBestChain = Option(BestChain(NonEmptyChain.fromChain(slotData.tail).get))

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
        .use { actor =>
          for {
            updatedState <- actor.send(BlockChecker.Message.RemoteSlotData(hostId, remoteSlotData))
            _ = assert(updatedState.bestKnownRemoteSlotDataOpt == expectedBestChain)
          } yield ()
        }
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
        arbitraryLinkedSlotDataChain.arbitrary.retryUntil(c => c.size > 3 && c.size < maxChainSize).first
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
        .onCall { case (id: BlockId, _: MonadThrow[F], _: Show[BlockId]) =>
          localSlotDataStore(id).pure[F]
        }

      (headerStore.contains _).expects(*).anyNumberOfTimes().onCall { id: BlockId => (id == localId).pure[F] }

      val currentBestChain = NonEmptyChain.fromSeq(slotData.toList.take(2)).get
      (chainSelectionAlgebra.compare _).expects(slotData.last, currentBestChain.last).returning(1.pure[F])

      val expectedRequest =
        PeersManager.Message.BlockHeadersRequest(
          hostId,
          NonEmptyChain.fromSeq(missedInChainData.take(chunkSize).map(_._1)).get
        ): PeersManager.Message
      (peersManager.sendNoWait _).expects(expectedRequest).once().returning(().pure[F])

      val expectedBestChain = Option(BestChain(NonEmptyChain.fromChain(slotData.tail).get))

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

  test("RemoteBlockHeader: Do not verify already known headers") {
    withMock {
      val headers: NonEmptyChain[BlockHeader] =
        nonEmptyChainArbOf(arbitraryHeader).arbitrary.retryUntil(c => c.size > 0 && c.size < maxChainSize).first
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

      (headerStore.contains _).expects(*).rep(headers.size.toInt).returning(true.pure[F])
      (headerValidation.validate _).expects(*, *).never()

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
        .use { actor =>
          for {
            _ <- actor.sendNoWait(BlockChecker.Message.RemoteBlockHeader(hostId, idAndHeaders))
          } yield ()
        }
    }
  }

  test("RemoteBlockHeader: Do not verify already known headers, stop processing if all headers are known") {
    withMock {
      val headers: NonEmptyChain[BlockHeader] =
        arbitraryLinkedBlockHeaderChain(Gen.choose(2, maxChainSize)).arbitrary.first
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

      (headerStore.contains _).expects(*).rep(headers.size.toInt).returning(true.pure[F])
      (headerValidation.validate _).expects(*, *).never()

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
        .use { actor =>
          for {
            _ <- actor.sendNoWait(BlockChecker.Message.RemoteBlockHeader(hostId, idAndHeaders))
          } yield ()
        }
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
        arbitraryLinkedBlockHeaderChain(Gen.choose(2, maxChainSize)).arbitrary.first
      val idAndHeaders = headers.map(h => (h.id, h))

      val knownHeadersSize = Gen.choose[Int](1, headers.size.toInt - 1).first
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
      (peersManager.sendNoWait _).expects(expectedMessage).returning(().pure[F])

      (peersManager.sendNoWait _).expects(*).never() // no other requests

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
        .use { actor =>
          for {
            _ <- actor.send(BlockChecker.Message.RemoteBlockHeader(hostId, idAndHeaders))
            _ = assert(newIdAndHeaders.map(_._1).forall(k => addedHeader.contains(k)))
          } yield ()
        }
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
        arbitraryLinkedBlockHeaderChain(Gen.choose(2, maxChainSize)).arbitrary.first
      val idAndHeaders = headers.map(h => (h.id, h))

      val knownHeadersSize = Gen.choose[Int](1, headers.size.toInt - 1).first
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
      (peersManager.sendNoWait _).expects(expectedMessage).returning(().pure[F])

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
        .onCall { case (id: BlockId, _: MonadThrow[F], _: Show[BlockId]) =>
          localSlotDataStore(id).pure[F]
        }

      val nextHeaders = newSlotData.toList.take(chunkSize).map(s => s.slotId.blockId)

      val nextHeaderMessage: PeersManager.Message =
        PeersManager.Message.BlockHeadersRequest(hostId, NonEmptyChain.fromSeq(nextHeaders).get)
      (peersManager.sendNoWait _).expects(nextHeaderMessage).once().returning(().pure[F])

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
        .use { actor =>
          for {
            _ <- actor.send(BlockChecker.Message.RemoteBlockHeader(hostId, idAndHeaders))
            _ = assert(newIdAndHeaders.map(_._1).forall(k => addedHeader.contains(k)))
          } yield ()
        }
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
      val ids: NonEmptyChain[BlockId] = bodies.map(_ => arbitraryHeader.arbitrary.first.id)

      val idAndBody: NonEmptyChain[(BlockId, BlockBody)] = ids.zipWith(bodies)((_, _))

      val message = BlockChecker.Message.RemoteBlockBodies(hostId, idAndBody)

      (bodyStore.contains _).expects(*).rep(bodies.size.toInt).returning(true.pure[F])
      (peersManager.sendNoWait _).expects(*).never()

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
        .use { actor =>
          for {
            _ <- actor.send(message)
          } yield ()
        }
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
      val ids: NonEmptyChain[BlockId] = bodies.map(_ => arbitraryHeader.arbitrary.first.id)

      val idAndBody: NonEmptyChain[(BlockId, BlockBody)] = ids.zipWith(bodies)((_, _))
      val idAndHeader: NonEmptyChain[(BlockId, BlockHeader)] =
        ids.map(id => (id, arbitraryHeader.arbitrary.first))

      val message = BlockChecker.Message.RemoteBlockBodies(hostId, idAndBody)

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
        .onCall { case (id: BlockId, _: MonadThrow[F], _: Show[BlockId]) =>
          headerStorageData(id).pure[F]
        }

      (headerToBodyValidation.validate _).expects(*).rep(newBodiesSize).onCall { b: Block =>
        Either.right[BlockHeaderToBodyValidationFailure, Block](b).pure[F]
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
        .onCall { case (_: AuthContext, b: BlockBody) =>
          Validated.validNec[BodyAuthorizationError, BlockBody](b).pure[F]
        }

      val storedBodies = mutable.Map.empty[BlockId, BlockBody]
      (bodyStore.put _).expects(*, *).rep(newBodiesSize).onCall { case (id: BlockId, block: BlockBody) =>
        storedBodies.put(id, block).pure[F].void
      }

      val lastBlockSlotData = arbitrarySlotData.arbitrary.first

      val lastBlockId = idAndHeader.last._2.id // shall be ids.last but we generate headers
      (slotDataStore.get _).expects(lastBlockId).returning(Option(lastBlockSlotData).pure[F])

      (localChain.isWorseThan _).expects(lastBlockSlotData).once().returning(false.pure[F])

      (peersManager.sendNoWait _).expects(*).never()

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
        .use { actor =>
          for {
            _ <- actor.send(message)
          } yield ()
        }
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
      val ids: NonEmptyChain[BlockId] = bodies.map(_ => arbitraryHeader.arbitrary.first.id)

      val idAndBody: NonEmptyChain[(BlockId, BlockBody)] = ids.zipWith(bodies)((_, _))
      val idAndHeader: NonEmptyChain[(BlockId, BlockHeader)] =
        ids.map(id => (id, arbitraryHeader.arbitrary.first))

      val message = BlockChecker.Message.RemoteBlockBodies(hostId, idAndBody)

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
        .onCall { case (id: BlockId, _: MonadThrow[F], _: Show[BlockId]) =>
          headerStorageData(id).pure[F]
        }

      (headerToBodyValidation.validate _).expects(*).rep(newBodiesSize).onCall { b: Block =>
        Either.right[BlockHeaderToBodyValidationFailure, Block](b).pure[F]
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
        .onCall { case (_: AuthContext, b: BlockBody) =>
          Validated.validNec[BodyAuthorizationError, BlockBody](b).pure[F]
        }

      val storedBodies = mutable.Map.empty[BlockId, BlockBody]
      (bodyStore.put _).expects(*, *).rep(newBodiesSize).onCall { case (id: BlockId, block: BlockBody) =>
        storedBodies.put(id, block).pure[F].void
      }

      val lastBlockSlotData = arbitrarySlotData.arbitrary.first

      val lastBlockId = idAndHeader.last._2.id // shall be ids.last but we generate headers
      (slotDataStore.get _).expects(lastBlockId).returning(Option(lastBlockSlotData).pure[F])

      (localChain.isWorseThan _).expects(lastBlockSlotData).once().returning(true.pure[F])
      (localChain.adopt _).expects(Validated.Valid(lastBlockSlotData)).once().returning(().pure[F])

      (peersManager.sendNoWait _).expects(*).never()

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
        .use { actor =>
          for {
            newState <- actor.send(message)
            _ = assert(newState.bestKnownRemoteSlotDataOpt.isEmpty)
          } yield ()
        }
    }
  }

}
