package co.topl.networking.fsnetwork

import cats.data.NonEmptyChain
import cats.effect.IO
import cats.implicits._
import co.topl.actor.Actor
import co.topl.algebras.Store
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.consensus.algebras.LocalChainAlgebra
import co.topl.consensus.models.{BlockHeader, SlotData}
import co.topl.eventtree.ParentChildTree
import co.topl.models.generators.consensus.ModelGenerators._
import co.topl.models.{IdentifierTypes, TypedBytes, TypedIdentifier}
import co.topl.networking.blockchain.BlockchainPeerClient
import co.topl.networking.fsnetwork.PeerBlockHeaderFetcherTest.F
import co.topl.typeclasses.implicits._
import fs2.Stream
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import scodec.bits.ByteVector
import co.topl.networking.fsnetwork.TestImplicits._

import scala.collection.mutable

object PeerBlockHeaderFetcherTest {
  type F[A] = IO[A]
}

class PeerBlockHeaderFetcherTest extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {
  implicit val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)

  val hostId: HostId = "127.0.0.1"
  val maxChainSize = 99

  test("Block header shall be downloaded by request") {
    withMock {
      val headers: NonEmptyChain[BlockHeader] =
        nonEmptyChainArbOf(arbitraryHeader).arbitrary.retryUntil(_.size < maxChainSize).sample.get
      val idAndHeader: NonEmptyChain[(TypedIdentifier, BlockHeader)] =
        headers.map(h => (h.id.asTypedBytes: TypedIdentifier, h))
      val idToHeader: Map[TypedIdentifier, BlockHeader] = idAndHeader.toList.toMap
      val blocksCount = idToHeader.size

      val client = mock[BlockchainPeerClient[F]]
      (client.getRemoteHeader _).expects(*).rep(blocksCount).onCall { id: TypedIdentifier =>
        idToHeader.get(id).pure[F]
      }

      val blockChecker = mock[Actor[F, BlockChecker.Message, BlockChecker.Response[F]]]
      val expectedMessage: BlockChecker.Message = BlockChecker.Message.RemoteBlockHeader(hostId, idAndHeader)
      (blockChecker.sendNoWait _).expects(expectedMessage).once().onCall { _: BlockChecker.Message => ().pure[F] }

      val localChain = mock[LocalChainAlgebra[F]]

      val slotDataStore = mock[Store[F, TypedIdentifier, SlotData]]

      val blockIdTree = mock[ParentChildTree[F, TypedIdentifier]]

      for {
        (actor, shutdown) <-
          PeerBlockHeaderFetcher
            .makeActor(hostId, client, blockChecker, localChain, slotDataStore, blockIdTree)
            .allocated
        _             <- actor.sendNoWait(PeerBlockHeaderFetcher.Message.DownloadBlockHeaders(idAndHeader.map(_._1)))
        shutdownFiber <- actor.gracefulShutdown(shutdown)
        _             <- shutdownFiber.join
      } yield ()

    }
  }

  test("New better slot data shall be sent if local chain is worse") {
    withMock {
      val slotData: NonEmptyChain[SlotData] =
        arbitrarySlotDataChain.arbitrary.retryUntil(c => c.size > 1 && c.size < maxChainSize).sample.get
      val idAndSlotData: NonEmptyChain[(TypedIdentifier, SlotData)] = slotData.map { s =>
        (TypedBytes(IdentifierTypes.Block.HeaderV2, ByteVector(s.slotId.blockId.value.toByteArray)): TypedIdentifier, s)
      }
      val (knownId, knownSlotData) = idAndSlotData.head
      val remoteSlotData = NonEmptyChain.fromChain(idAndSlotData.tail).get

      val (bestSlotId, bestSlotData) = remoteSlotData.last
      val remoteIdToSlotData: Map[TypedIdentifier, SlotData] = remoteSlotData.toList.toMap
      val remoteSlotDataCount = remoteIdToSlotData.size

      val client = mock[BlockchainPeerClient[F]]
      (client.getRemoteSlotData _).expects(*).rep(remoteSlotDataCount).onCall { id: TypedIdentifier =>
        remoteIdToSlotData.get(id).pure[F]
      }
      (client.remotePeerAdoptions _).expects().once().onCall { () =>
        Stream.eval[F, TypedIdentifier](bestSlotId.pure[F]).pure[F]
      }

      val blockChecker = mock[Actor[F, BlockChecker.Message, BlockChecker.Response[F]]]
      val expectedMessage: BlockChecker.Message = BlockChecker.Message.RemoteSlotData(hostId, remoteSlotData.map(_._2))
      (blockChecker.sendNoWait _).expects(expectedMessage).once().onCall { _: BlockChecker.Message => ().pure[F] }

      val localChain = mock[LocalChainAlgebra[F]]
      (localChain.isWorseThan _).expects(bestSlotData).once().onCall { _: SlotData => true.pure[F] }

      val slotDataStoreMap = mutable.Map.empty[TypedIdentifier, SlotData]
      val slotDataStore = mock[Store[F, TypedIdentifier, SlotData]]
      (slotDataStore.get _).expects(*).rep(remoteSlotDataCount + 1).onCall { id: TypedIdentifier =>
        { if (id === knownId) Option(knownSlotData) else None }.pure[F]
      }
      (slotDataStore.contains _).expects(*).rep(remoteSlotDataCount + 2).onCall { id: TypedIdentifier =>
        (!remoteIdToSlotData.contains(id)).pure[F]
      }
      (slotDataStore.put _).expects(*, *).rep(remoteSlotDataCount).onCall {
        case (id: TypedIdentifier, slotData: SlotData) => slotDataStoreMap.put(id, slotData).pure[F].void
      }

      val blockIdTree = mock[ParentChildTree[F, TypedIdentifier]]
      (blockIdTree.associate _).expects(*, *).rep(remoteSlotDataCount).onCall(_ => ().pure[F])

      for {
        (actor, shutdown) <- PeerBlockHeaderFetcher
          .makeActor(hostId, client, blockChecker, localChain, slotDataStore, blockIdTree)
          .allocated
        _             <- actor.send(PeerBlockHeaderFetcher.Message.StartActor)
        shutdownFiber <- actor.gracefulShutdown(shutdown)
        _             <- shutdownFiber.join
      } yield ()

    }
  }

  test("New better slot data shall not be sent if local chain is better") {
    withMock {
      val slotData: NonEmptyChain[SlotData] =
        arbitrarySlotDataChain.arbitrary.retryUntil(c => c.size > 1 && c.size < maxChainSize).sample.get
      val idAndSlotData: NonEmptyChain[(TypedIdentifier, SlotData)] = slotData.map { s =>
        (TypedBytes(IdentifierTypes.Block.HeaderV2, ByteVector(s.slotId.blockId.value.toByteArray)): TypedIdentifier, s)
      }
      val (knownId, knownSlotData) = idAndSlotData.head
      val remoteSlotData = NonEmptyChain.fromChain(idAndSlotData.tail).get

      val (bestSlotId, bestSlotData) = remoteSlotData.last
      val remoteIdToSlotData: Map[TypedIdentifier, SlotData] = remoteSlotData.toList.toMap
      val remoteSlotDataCount = remoteIdToSlotData.size

      val client = mock[BlockchainPeerClient[F]]
      (client.getRemoteSlotData _).expects(*).rep(remoteSlotDataCount).onCall { id: TypedIdentifier =>
        remoteIdToSlotData.get(id).pure[F]
      }
      (client.remotePeerAdoptions _).expects().once().onCall { () =>
        Stream.eval[F, TypedIdentifier](bestSlotId.pure[F]).pure[F]
      }

      val blockChecker = mock[Actor[F, BlockChecker.Message, BlockChecker.Response[F]]]
      val expectedMessage: BlockChecker.Message = BlockChecker.Message.RemoteSlotData(hostId, remoteSlotData.map(_._2))
      (blockChecker.sendNoWait _).expects(expectedMessage).never()

      val localChain = mock[LocalChainAlgebra[F]]
      (localChain.isWorseThan _).expects(bestSlotData).once().onCall { _: SlotData => false.pure[F] }

      val slotDataStoreMap = mutable.Map.empty[TypedIdentifier, SlotData]
      val slotDataStore = mock[Store[F, TypedIdentifier, SlotData]]
      (slotDataStore.get _).expects(*).rep(remoteSlotDataCount + 1).onCall { id: TypedIdentifier =>
        {
          if (id === knownId) Option(knownSlotData) else None
        }.pure[F]
      }
      (slotDataStore.contains _).expects(*).rep(remoteSlotDataCount + 2).onCall { id: TypedIdentifier =>
        (!remoteIdToSlotData.contains(id)).pure[F]
      }
      (slotDataStore.put _).expects(*, *).rep(remoteSlotDataCount).onCall {
        case (id: TypedIdentifier, slotData: SlotData) => slotDataStoreMap.put(id, slotData).pure[F].void
      }

      val blockIdTree = mock[ParentChildTree[F, TypedIdentifier]]
      (blockIdTree.associate _).expects(*, *).rep(remoteSlotDataCount).onCall(_ => ().pure[F])

      for {
        (actor, shutdown) <- PeerBlockHeaderFetcher
          .makeActor(hostId, client, blockChecker, localChain, slotDataStore, blockIdTree)
          .allocated
        _             <- actor.send(PeerBlockHeaderFetcher.Message.StartActor)
        shutdownFiber <- actor.gracefulShutdown(shutdown)
        _             <- shutdownFiber.join
      } yield ()

    }
  }
}
