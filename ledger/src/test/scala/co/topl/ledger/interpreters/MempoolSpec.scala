package co.topl.ledger.interpreters

import cats.data.{Chain, NonEmptyChain}
import cats.effect.{Deferred, IO, MonadCancel}
import cats.implicits._
import cats.{Applicative, MonadThrow}
import co.topl.algebras.ClockAlgebra
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.eventtree.ParentChildTree
import co.topl.models.ModelGenerators._
import co.topl.models.{BlockBodyV2, Slot, Transaction, TypedIdentifier}
import co.topl.typeclasses.implicits._
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.{Gen, Test}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory

import scala.collection.immutable.ListSet
import scala.concurrent.duration._

class MempoolSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  override def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters
      .withMaxSize(3)

  test("expose a Set of Transaction IDs at a specific block") {
    PropF.forAllF(
      nonEmptyChainOf[(TypedIdentifier, NonEmptyChain[Transaction])](
        Gen.zip(arbitraryTypedIdentifier.arbitrary, nonEmptyChainOf(arbitraryTransaction.arbitrary))
      ),
      arbitraryTransaction.arbitrary
    ) { case (bodies, newTx) =>
      withMock {
        val bodiesMap = bodies.toList.toMap
        val transactions = bodies.flatMap(_._2).toList.map(t => t.id.asTypedBytes -> t).toMap.updated(newTx.id, newTx)
        val fetchBody =
          (id: TypedIdentifier) => ListSet.from(bodiesMap(id).map(_.id: TypedIdentifier).toIterable).pure[F]
        val fetchTransaction = (id: TypedIdentifier) => transactions(id).pure[F]
        val clock = mock[ClockAlgebra[F]]
        (() => clock.globalSlot)
          .expects()
          .anyNumberOfTimes()
          .returning(0L.pure[F])
        (clock
          .delayedUntilSlot(_: Slot))
          .expects(*)
          .anyNumberOfTimes()
          .returning(MonadCancel[F].never[Unit])
        for {
          tree <- ParentChildTree.FromRef.make[F, TypedIdentifier]
          _    <- bodies.map(_._1).sliding2.traverse { case (id1, id2) => tree.associate(id2, id1) }
          underTest <- Mempool
            .make[F](
              bodies.head._1.pure[F],
              fetchBody,
              fetchTransaction,
              tree,
              clock,
              _ => Applicative[F].unit,
              Long.MaxValue,
              Long.MaxValue
            )

          _ <- underTest.read(bodies.last._1).assertEquals(Set.empty[TypedIdentifier])
          _ <- underTest.add(newTx.id)
          _ <- underTest.read(bodies.last._1).assertEquals(Set(newTx.id.asTypedBytes))
          _ <- underTest.remove(newTx.id)
          _ <- underTest.read(bodies.last._1).assertEquals(Set.empty[TypedIdentifier])
          _ <- underTest
            .read(bodies.head._1)
            .assertEquals(
              bodies.tail.toList
                .flatMap(_._2.toList.map(_.id.asTypedBytes))
                .toSet
            )
        } yield ()
      }
    }
  }

  test("allow transactions to be added externally") {
    PropF.forAllF { (currentBlockId: TypedIdentifier, transaction: Transaction) =>
      withMock {
        val fetchTransaction = mockFunction[TypedIdentifier, F[Transaction]]
        fetchTransaction
          .expects(transaction.id: TypedIdentifier)
          .once()
          .returning(transaction.pure[F])
        val clock = mock[ClockAlgebra[F]]
        (() => clock.globalSlot)
          .expects()
          .once()
          .returning(0L.pure[F])
        (clock
          .delayedUntilSlot(_: Slot))
          .expects(*)
          .once()
          .returning(MonadCancel[F].never[Unit])
        for {
          underTest <- Mempool
            .make[F](
              currentBlockId.pure[F],
              mockFunction[TypedIdentifier, F[BlockBodyV2]],
              fetchTransaction,
              mock[ParentChildTree[F, TypedIdentifier]],
              clock,
              _ => Applicative[F].unit,
              Long.MaxValue,
              Long.MaxValue
            )

          _ <- underTest.add(transaction.id).assert
        } yield ()
      }
    }
  }

  test("expire transactions at the user-defined slot") {
    PropF.forAllF { (currentBlockId: TypedIdentifier, transactionWithRandomTime: Transaction) =>
      withMock {
        val transaction =
          transactionWithRandomTime.copy(schedule = transactionWithRandomTime.schedule.copy(maximumSlot = 2))
        val fetchTransaction = mockFunction[TypedIdentifier, F[Transaction]]
        fetchTransaction
          .expects(transaction.id: TypedIdentifier)
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
              .delayedUntilSlot(_: Slot))
              // This is the real thing being tested here
              .expects(transaction.schedule.maximumSlot)
              .once()
              .returning(deferred.get)
          underTest <-
            Mempool
              .make[F](
                currentBlockId.pure[F],
                mockFunction[TypedIdentifier, F[BlockBodyV2]],
                fetchTransaction,
                mock[ParentChildTree[F, TypedIdentifier]],
                clock,
                _ => Applicative[F].unit,
                Long.MaxValue,
                Long.MaxValue
              )

          _ <- underTest.add(transaction.id)
          _ <- underTest.read(currentBlockId).assertEquals(Set(transaction.id.asTypedBytes))
          _ <- deferred.complete(())
          _ <- retry(underTest.read(currentBlockId).assertEquals(Set.empty[TypedIdentifier]))
        } yield ()
      }
    }
  }

  test("expire transactions at the node-configured slot if the user-defined slot is too high") {
    PropF.forAllF { (currentBlockId: TypedIdentifier, transactionWithRandomTime: Transaction) =>
      withMock {
        val transaction =
          transactionWithRandomTime.copy(schedule =
            transactionWithRandomTime.schedule.copy(maximumSlot = Long.MaxValue)
          )
        val fetchTransaction = mockFunction[TypedIdentifier, F[Transaction]]
        fetchTransaction
          .expects(transaction.id: TypedIdentifier)
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
              .delayedUntilSlot(_: Slot))
              // This is the real thing being tested here
              .expects(100L)
              .once()
              .returning(deferred.get)
          underTest <-
            Mempool
              .make[F](
                currentBlockId.pure[F],
                mockFunction[TypedIdentifier, F[BlockBodyV2]],
                fetchTransaction,
                mock[ParentChildTree[F, TypedIdentifier]],
                clock,
                _ => Applicative[F].unit,
                100L,
                Long.MaxValue
              )

          _ <- underTest.add(transaction.id)
          _ <- underTest.read(currentBlockId).assertEquals(Set(transaction.id.asTypedBytes))
          _ <- deferred.complete(())
          _ <- retry(underTest.read(currentBlockId).assertEquals(Set.empty[TypedIdentifier]))
        } yield ()
      }
    }
  }

  // TODO: Re-enable once it is understood why this test is flaky
  // (Note: Occasionally, this test fails due to a 30s Future timeout)
  test("expire a transaction more aggressively when a double-spend is detected".ignore) {
    PropF.forAllF {
      (
        baseTransaction: Transaction,
        input:           Transaction.Input,
        ancestorBlockId: TypedIdentifier,
        blockIdA:        TypedIdentifier,
        blockIdB:        TypedIdentifier
      ) =>
        withMock {
          val transactionWithSingleInput = baseTransaction.copy(inputs = Chain(input))
          // Transaction A and Transaction B are exactly the same, except for the creation timestamp to force a different ID
          val transactionA =
            transactionWithSingleInput.copy(schedule = transactionWithSingleInput.schedule.copy(creation = 0))
          val transactionAId = transactionA.id.asTypedBytes
          val transactionB =
            transactionWithSingleInput.copy(schedule = transactionWithSingleInput.schedule.copy(creation = 1))
          val transactionBId = transactionB.id.asTypedBytes
          val bodies =
            Map(
              blockIdA -> ListSet(transactionAId),
              blockIdB -> ListSet(transactionBId)
            )
          val transactions =
            Map(
              transactionAId -> transactionA,
              transactionBId -> transactionB
            )

          val fetchBody = (id: TypedIdentifier) => bodies(id).pure[F]
          val fetchTransaction = (id: TypedIdentifier) => transactions(id).pure[F]
          val clock = mock[ClockAlgebra[F]]
          (() => clock.globalSlot)
            .expects()
            .anyNumberOfTimes()
            .returning(0L.pure[F])
          for {
            tree <- ParentChildTree.FromRef.make[F, TypedIdentifier]
            _    <- tree.associate(blockIdA, ancestorBlockId)
            _    <- tree.associate(blockIdB, ancestorBlockId)
            underTest <- Mempool
              .make[F](
                ancestorBlockId.pure[F],
                fetchBody,
                fetchTransaction,
                tree,
                clock,
                _ => Applicative[F].unit,
                Long.MaxValue,
                1L
              )
            _ <- underTest.read(blockIdA).assertEquals(Set.empty[TypedIdentifier])
            _ <-
              inSequence {
                if (transactionA.schedule.maximumSlot != 1L) {
                  // The "unapply" operation will schedule a normal expiration
                  (clock
                    .delayedUntilSlot(_: Slot))
                    .expects(transactionA.schedule.maximumSlot)
                    .once()
                    .returning(MonadCancel[F].never[Unit])
                  // But the "apply" operation will re-schedule the expiration
                  (clock
                    .delayedUntilSlot(_: Slot))
                    .expects(1L)
                    .once()
                    .returning(MonadCancel[F].never[Unit])
                } else {
                  // This is just an edge-case where the Schedule generator produces a maximum slot of 1L,
                  // so the scalamock expectation needs to expect the value twice instead of just once
                  (clock
                    .delayedUntilSlot(_: Slot))
                    .expects(1L)
                    .twice()
                    .returning(MonadCancel[F].never[Unit])
                }
                underTest.read(blockIdB).assertEquals(Set(transactionAId))
              }
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
