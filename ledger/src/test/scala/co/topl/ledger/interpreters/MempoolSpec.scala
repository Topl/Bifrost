package co.topl.ledger.interpreters

import cats.Applicative
import cats.MonadThrow
import cats.data.NonEmptyChain
import cats.effect._
import cats.implicits._
import co.topl.algebras.ClockAlgebra
import co.topl.brambl.generators.ModelGenerators._
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction._
import co.topl.brambl.syntax._
import co.topl.consensus.models.BlockId
import co.topl.eventtree.ParentChildTree
import co.topl.models.ModelGenerators._
import co.topl.models.generators.consensus.ModelGenerators._
import co.topl.node.models.BlockBody
import co.topl.typeclasses.implicits._
import munit.CatsEffectSuite
import munit.ScalaCheckEffectSuite
import org.scalacheck.Gen
import org.scalacheck.Test
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory

import scala.concurrent.duration._

class MempoolSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  override def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters
      .withMaxSize(3)
      .withMinSuccessfulTests(10)

  test("expose a Set of Transaction IDs at a specific block") {
    PropF.forAllF(
      nonEmptyChainOf[(BlockId, NonEmptyChain[IoTransaction])](
        Gen.zip(arbitraryBlockId.arbitrary, nonEmptyChainOf(arbitraryIoTransaction.arbitrary.map(_.embedId)))
      ),
      arbitraryIoTransaction.arbitrary.map(_.embedId)
    ) { case (bodies, newTx: IoTransaction) =>
      withMock {
        val bodiesMap = bodies.toList.toMap
        val transactions = bodies.flatMap(_._2).toList.map(t => t.id -> t).toMap.updated(newTx.id, newTx)
        val fetchBody =
          (id: BlockId) => BlockBody(bodiesMap(id).map(_.id).toList).pure[F]
        val fetchTransaction = (id: TransactionId) => transactions(id).pure[F]
        val clock = mock[ClockAlgebra[F]]
        (() => clock.globalSlot)
          .expects()
          .anyNumberOfTimes()
          .returning(0L.pure[F])
        (clock
          .delayedUntilSlot(_: Long))
          .expects(*)
          .anyNumberOfTimes()
          .returning(MonadCancel[F].never[Unit])
        for {
          tree <- ParentChildTree.FromRef.make[F, BlockId]
          _    <- bodies.map(_._1).sliding2.traverse { case (id1, id2) => tree.associate(id2, id1) }
          _ <- Mempool
            .make[F](
              bodies.head._1.pure[F],
              fetchBody,
              fetchTransaction,
              tree,
              _ => Applicative[F].unit,
              clock,
              _ => Applicative[F].unit,
              Long.MaxValue,
              Long.MaxValue
            )
            .use(underTest =>
              for {
                _ <- underTest.read(bodies.last._1).assertEquals(Set.empty[TransactionId])
                _ <- underTest.add(newTx.id)
                _ <- underTest.read(bodies.last._1).assertEquals(Set(newTx.id))
                _ <- underTest.remove(newTx.id)
                _ <- underTest.read(bodies.last._1).assertEquals(Set.empty[TransactionId])
                _ <- underTest
                  .read(bodies.head._1)
                  .assertEquals(
                    bodies.tail.toList
                      .flatMap(_._2.toList.map(_.id))
                      .toSet
                  )
              } yield ()
            )
        } yield ()
      }
    }
  }

  test("allow transactions to be added externally") {
    PropF.forAllF { (currentBlockId: BlockId, _transaction: IoTransaction) =>
      val transaction = _transaction.embedId
      withMock {
        val fetchTransaction = mockFunction[TransactionId, F[IoTransaction]]
        fetchTransaction
          .expects(transaction.id)
          .once()
          .returning(transaction.pure[F])
        val clock = mock[ClockAlgebra[F]]
        (() => clock.globalSlot)
          .expects()
          .once()
          .returning(0L.pure[F])
        (clock
          .delayedUntilSlot(_: Long))
          .expects(*)
          .once()
          .returning(MonadCancel[F].never[Unit])
        for {
          _ <- Mempool
            .make[F](
              currentBlockId.pure[F],
              mockFunction[BlockId, F[BlockBody]],
              fetchTransaction,
              mock[ParentChildTree[F, BlockId]],
              _ => Applicative[F].unit,
              clock,
              _ => Applicative[F].unit,
              Long.MaxValue,
              Long.MaxValue
            )
            .use(_.add(transaction.id).assert)
        } yield ()
      }
    }
  }

  test("expire transactions at the user-defined slot") {
    PropF.forAllF { (currentBlockId: BlockId, transactionWithRandomTime: IoTransaction) =>
      withMock {
        val transaction = transactionWithRandomTime.update(_.datum.event.schedule.max.set(2)).embedId
        val fetchTransaction = mockFunction[TransactionId, F[IoTransaction]]
        fetchTransaction
          .expects(transaction.id)
          .once()
          .returning(transaction.pure[F])
        val clock = mock[ClockAlgebra[F]]
        (() => clock.globalSlot)
          .expects()
          .once()
          .returning(0L.pure[F])
        for {
          deferred <- Deferred[F, Unit]
          _ =
            (clock
              .delayedUntilSlot(_: Long))
              // This is the real thing being tested here
              .expects(transaction.datum.event.schedule.max)
              .once()
              .returning(deferred.get)
          _ <-
            Mempool
              .make[F](
                currentBlockId.pure[F],
                mockFunction[BlockId, F[BlockBody]],
                fetchTransaction,
                mock[ParentChildTree[F, BlockId]],
                _ => Applicative[F].unit,
                clock,
                _ => Applicative[F].unit,
                Long.MaxValue,
                Long.MaxValue
              )
              .use(underTest =>
                for {
                  _ <- underTest.add(transaction.id)
                  _ <- underTest.read(currentBlockId).assertEquals(Set(transaction.id))
                  _ <- deferred.complete(())
                  _ <- retry(underTest.read(currentBlockId).assertEquals(Set.empty[TransactionId]))
                } yield ()
              )
        } yield ()
      }
    }
  }

  test("expire transactions at the node-configured slot if the user-defined slot is too high") {
    PropF.forAllF { (currentBlockId: BlockId, transactionWithRandomTime: IoTransaction) =>
      withMock {
        val transaction =
          transactionWithRandomTime.update(_.datum.event.schedule.max.set(Long.MaxValue)).embedId
        val fetchTransaction = mockFunction[TransactionId, F[IoTransaction]]
        fetchTransaction
          .expects(transaction.id)
          .once()
          .returning(transaction.pure[F])
        val clock = mock[ClockAlgebra[F]]
        (() => clock.globalSlot)
          .expects()
          .once()
          .returning(0L.pure[F])
        for {
          deferred <- Deferred[F, Unit]
          _ =
            (clock
              .delayedUntilSlot(_: Long))
              // This is the real thing being tested here
              .expects(100L)
              .once()
              .returning(deferred.get)
          _ <-
            Mempool
              .make[F](
                currentBlockId.pure[F],
                mockFunction[BlockId, F[BlockBody]],
                fetchTransaction,
                mock[ParentChildTree[F, BlockId]],
                _ => Applicative[F].unit,
                clock,
                _ => Applicative[F].unit,
                100L,
                Long.MaxValue
              )
              .use(underTest =>
                for {
                  _ <- underTest.add(transaction.id)
                  _ <- underTest.read(currentBlockId).assertEquals(Set(transaction.id))
                  _ <- deferred.complete(())
                  _ <- retry(underTest.read(currentBlockId).assertEquals(Set.empty[TransactionId]))
                } yield ()
              )

        } yield ()
      }
    }
  }

  test("expire a transaction more aggressively when a double-spend is detected") {
    PropF.forAllF {
      (
        baseTransaction: IoTransaction,
        input:           SpentTransactionOutput,
        ancestorBlockId: BlockId,
        blockIdA:        BlockId,
        blockIdB:        BlockId
      ) =>
        withMock {
          val transactionWithSingleInput = baseTransaction.addInputs(input)
          // Transaction A and Transaction B are exactly the same, except for the creation schedule to force a different ID
          val transactionA = transactionWithSingleInput.update(_.datum.event.schedule.set(Schedule(0, 100))).embedId
          val transactionB = transactionWithSingleInput.update(_.datum.event.schedule.set(Schedule(1, 100))).embedId
          val bodies =
            Map(
              blockIdA -> List(transactionA.id),
              blockIdB -> List(transactionB.id)
            )
          val transactions =
            Map(
              transactionA.id -> transactionA,
              transactionB.id -> transactionB
            )

          val fetchBody = (id: BlockId) => BlockBody(bodies(id)).pure[F]
          val fetchTransaction = (id: TransactionId) => transactions(id).pure[F]
          val clock = mock[ClockAlgebra[F]]
          (() => clock.globalSlot)
            .expects()
            .anyNumberOfTimes()
            .returning(0L.pure[F])
          for {
            tree <- ParentChildTree.FromRef.make[F, BlockId]
            _    <- tree.associate(blockIdA, ancestorBlockId)
            _    <- tree.associate(blockIdB, ancestorBlockId)
            _ <- Mempool
              .make[F](
                ancestorBlockId.pure[F],
                fetchBody,
                fetchTransaction,
                tree,
                _ => Applicative[F].unit,
                clock,
                _ => Applicative[F].unit,
                Long.MaxValue,
                1L
              )
              .use(underTest =>
                for {
                  _ <- underTest.read(blockIdA).assertEquals(Set.empty[TransactionId])
                  _ <-
                    inSequence {
                      if (transactionA.datum.event.schedule.max != 1L) {
                        // The "unapply" operation will schedule a normal expiration
                        (clock
                          .delayedUntilSlot(_: Long))
                          .expects(transactionA.datum.event.schedule.max)
                          .once()
                          .returning(MonadCancel[F].never[Unit])
                        // But the "apply" operation will re-schedule the expiration
                        (clock
                          .delayedUntilSlot(_: Long))
                          .expects(1L)
                          .once()
                          .returning(MonadCancel[F].never[Unit])
                      } else {
                        // This is just an edge-case where the Schedule generator produces a maximum slot of 1L,
                        // so the scalamock expectation needs to expect the value twice instead of just once
                        (clock
                          .delayedUntilSlot(_: Long))
                          .expects(1L)
                          .twice()
                          .returning(MonadCancel[F].never[Unit])
                      }
                      underTest.read(blockIdB).assertEquals(Set(transactionA.id))
                    }
                } yield ()
              )
          } yield ()

        }
    }
  }

  private def retry[A](f: => F[A], attempts: Int = 5, delay: FiniteDuration = 100.milli): F[A] =
    if (attempts > 1)
      MonadThrow[F].recoverWith(f) { case _ => IO.sleep(delay) >> retry(f, attempts - 1, delay) }
    else
      f

}
