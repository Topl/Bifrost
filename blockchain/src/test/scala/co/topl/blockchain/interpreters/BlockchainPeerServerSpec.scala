package co.topl.blockchain.interpreters

import cats.effect.{IO, Resource}
import co.topl.consensus.algebras.LocalChainAlgebra
import co.topl.eventtree.EventSourcedState
import co.topl.ledger.algebras.MempoolAlgebra
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory
import co.topl.models.ModelGenerators._
import co.topl.consensus.models._
import co.topl.node.models.BlockBody
import co.topl.models.generators.node.ModelGenerators._
import co.topl.models.generators.consensus.ModelGenerators._
import fs2._
import fs2.concurrent.Topic
import org.scalacheck.effect.PropF
import cats.implicits._
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.generators.ModelGenerators._
import co.topl.ledger.models.MempoolGraph
import co.topl.networking.NetworkGen._

import scala.concurrent.duration._

class BlockchainPeerServerSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  override val munitTimeout: FiniteDuration = 5.seconds

  test("serve slot data") {
    PropF.forAllF { slotData: SlotData =>
      withMock {
        val f = mockFunction[BlockId, F[Option[SlotData]]]
        (f.expects(slotData.slotId.blockId).once().returning(slotData.some.pure[F]))
        for {
          _ <- makeServer(fetchSlotData = f)
            .use(underTest => underTest.getLocalSlotData(slotData.slotId.blockId).assertEquals(slotData.some))
        } yield ()
      }
    }
  }

  test("serve headers") {
    PropF.forAllF { (header: BlockHeader, id: BlockId) =>
      withMock {
        val f = mockFunction[BlockId, F[Option[BlockHeader]]]
        (f.expects(id).once().returning(header.some.pure[F]))
        for {
          _ <- makeServer(fetchHeader = f)
            .use(underTest => underTest.getLocalHeader(id).assertEquals(header.some))
        } yield ()
      }
    }
  }

  test("serve bodies") {
    PropF.forAllF { (body: BlockBody, id: BlockId) =>
      withMock {
        val f = mockFunction[BlockId, F[Option[BlockBody]]]
        (f.expects(id).once().returning(body.some.pure[F]))
        for {
          _ <- makeServer(fetchBody = f)
            .use(underTest => underTest.getLocalBody(id).assertEquals(body.some))
        } yield ()
      }
    }
  }

  test("serve transactions") {
    PropF.forAllF { (transaction: IoTransaction, id: TransactionId) =>
      withMock {
        val f = mockFunction[TransactionId, F[Option[IoTransaction]]]
        (f.expects(id).once().returning(transaction.some.pure[F]))
        for {
          _ <- makeServer(fetchTransaction = f)
            .use(underTest => underTest.getLocalTransaction(id).assertEquals(transaction.some))
        } yield ()
      }
    }
  }

  test("serve block ID at height") {
    PropF.forAllF { (height: Long, head: SlotData, resultId: BlockId) =>
      withMock {
        val localChain = mock[LocalChainAlgebra[F]]
        (() => localChain.head).expects().once().returning(head.pure[F])
        val blockHeights = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]
        (blockHeights
          .useStateAt[Option[BlockId]](_: BlockId)(
            _: (Long => F[Option[BlockId]]) => F[Option[BlockId]]
          ))
          .expects(head.slotId.blockId, *)
          .once()
          .returning(resultId.some.pure[F])
        for {
          _ <- makeServer(localChain = localChain, blockHeights = blockHeights)
            .use(underTest => underTest.getLocalBlockAtHeight(height).assertEquals(resultId.some))
        } yield ()
      }
    }
  }

  test("serve local block adoptions") {
    PropF.forAllF { (head: SlotData, adoptionA: BlockId, adoptionB: BlockId, adoptionC: BlockId) =>
      withMock {
        val localChain = mock[LocalChainAlgebra[F]]
        (() => localChain.head).expects().once().returning(head.pure[F])
        for {
          topic <- Topic[F, BlockId]
          publisher = Stream(adoptionA, adoptionB, adoptionC).through(topic.publish)
          result <- makeServer(localChain = localChain, newBlockIds = topic.pure[F])
            .use(server => Stream.force(server.localBlockAdoptions).concurrently(publisher).compile.toList)
          _ <- IO(result).assertEquals(List(head.slotId.blockId, adoptionA, adoptionB, adoptionC))
        } yield ()
      }
    }
  }

  test("serve local transaction notifications") {
    PropF.forAllF {
      (
        head:       SlotData,
        mempoolTxA: TransactionId,
        mempoolTxB: TransactionId,
        mempoolTxC: TransactionId,
        adoptionA:  TransactionId,
        adoptionB:  TransactionId,
        adoptionC:  TransactionId
      ) =>
        withMock {
          val currentMempool = MempoolGraph(
            Map(
              mempoolTxA -> IoTransaction.defaultInstance,
              mempoolTxB -> IoTransaction.defaultInstance,
              mempoolTxC -> IoTransaction.defaultInstance
            ),
            Map.empty,
            Map.empty
          )
          val mempool = mock[MempoolAlgebra[F]]
          (mempool.read _).expects(head.slotId.blockId).once().returning(currentMempool.pure[F])
          val localChain = mock[LocalChainAlgebra[F]]
          (() => localChain.head).expects().once().returning(head.pure[F])
          for {
            topic <- Topic[F, TransactionId]
            publisher = Stream(adoptionA, adoptionB, adoptionC).through(topic.publish)
            result <- makeServer(localChain = localChain, mempool = mempool, newTransactionIds = topic.pure[F])
              .use(server => Stream.force(server.localTransactionNotifications).concurrently(publisher).compile.toList)
            _ <- IO(result).assertEquals(List(mempoolTxA, mempoolTxB, mempoolTxC, adoptionA, adoptionB, adoptionC))
          } yield ()
        }
    }
  }

  private def makeServer(
    fetchSlotData:    BlockId => F[Option[SlotData]] = _ => ???,
    fetchHeader:      BlockId => F[Option[BlockHeader]] = _ => ???,
    fetchBody:        BlockId => F[Option[BlockBody]] = _ => ???,
    fetchTransaction: TransactionId => F[Option[IoTransaction]] = _ => ???,
    blockHeights: EventSourcedState[F, Long => F[Option[BlockId]], BlockId] =
      mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]],
    localChain:        LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]],
    mempool:           MempoolAlgebra[F] = mock[MempoolAlgebra[F]],
    newBlockIds:       F[Topic[F, BlockId]] = Topic[F, BlockId],
    newTransactionIds: F[Topic[F, TransactionId]] = Topic[F, TransactionId]
  ) =
    (Resource.eval(newBlockIds), Resource.eval(newTransactionIds)).tupled
      .flatMap { case (newBlockIds, newTransactionIds) =>
        BlockchainPeerServer
          .make(
            fetchSlotData,
            fetchHeader,
            fetchBody,
            fetchTransaction,
            blockHeights,
            localChain,
            mempool,
            newBlockIds,
            newTransactionIds
          )(arbitraryConnectedPeer.arbitrary.first)
      }
}
