package co.topl.ledger.interpreters

import cats.{Applicative, MonadThrow}
import cats.data.{Chain, NonEmptyChain}
import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO, MonadCancel}
import cats.implicits._
import co.topl.algebras.ClockAlgebra
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.eventtree.ParentChildTree
import co.topl.models.ModelGenerators._
import co.topl.models.{BlockBodyV2, Slot, Transaction, TypedIdentifier}
import co.topl.typeclasses.implicits._
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{EitherValues, OptionValues}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.concurrent.duration._

class MempoolSpec
    extends AnyFlatSpec
    with Matchers
    with ScalaCheckPropertyChecks
    with EitherValues
    with OptionValues
    with MockFactory {

  behavior of "Mempool"

  type F[A] = IO[A]

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(sizeRange = 3)

  it should "expose a Set of Transaction IDs at a specific block" in {
    forAll(
      nonEmptyChainOf[(TypedIdentifier, NonEmptyChain[Transaction])](
        Gen.zip(arbitraryTypedIdentifier.arbitrary, nonEmptyChainOf(arbitraryTransaction.arbitrary))
      ),
      arbitraryTransaction.arbitrary
    ) { case (bodies, newTx) =>
      val bodiesMap = bodies.toList.toMap
      val transactions = bodies.flatMap(_._2).toList.map(t => t.id.asTypedBytes -> t).toMap.updated(newTx.id, newTx)
      val fetchBody = (id: TypedIdentifier) => bodiesMap(id).map(_.id: TypedIdentifier).toList.pure[F]
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
      val tree = ParentChildTree.FromRef.make[F, TypedIdentifier].unsafeRunSync()
      bodies.map(_._1).sliding2.foreach { case (id1, id2) => tree.associate(id2, id1).unsafeRunSync() }
      val underTest =
        Mempool
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
          .unsafeRunSync()

      underTest.read(bodies.last._1).unsafeRunSync() shouldBe Set.empty[TypedIdentifier]
      underTest.add(newTx.id).unsafeRunSync()
      underTest.read(bodies.last._1).unsafeRunSync() shouldBe Set(newTx.id.asTypedBytes)
      underTest.remove(newTx.id).unsafeRunSync()
      underTest.read(bodies.last._1).unsafeRunSync() shouldBe Set.empty[TypedIdentifier]
      underTest.read(bodies.head._1).unsafeRunSync() shouldBe bodies.tail.toList
        .flatMap(_._2.toList.map(_.id.asTypedBytes))
        .toSet
    }
  }

  it should "allow transactions to be added externally" in {
    forAll { (currentBlockId: TypedIdentifier, transaction: Transaction) =>
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
      val underTest =
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
          .unsafeRunSync()

      underTest.add(transaction.id).unsafeRunSync()
    }
  }

  it should "expire transactions at the user-defined slot" in {
    forAll { (currentBlockId: TypedIdentifier, transactionWithRandomTime: Transaction) =>
      val transaction =
        transactionWithRandomTime.copy(chronology = transactionWithRandomTime.chronology.copy(maximumSlot = 2))
      val deferred = Deferred[F, Unit].unsafeRunSync()
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
        // This is the real thing being tested here
        .expects(transaction.chronology.maximumSlot)
        .once()
        .returning(deferred.get)
      val underTest =
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
          .unsafeRunSync()

      underTest.add(transaction.id).unsafeRunSync()
      underTest.read(currentBlockId).unsafeRunSync() shouldBe Set(transaction.id.asTypedBytes)
      deferred.complete(()).unsafeRunSync()
      retry(underTest.read(currentBlockId).map(_ shouldBe Set.empty[TypedIdentifier])).unsafeRunSync()
    }
  }

  it should "expire transactions at the node-configured slot if the user-defined slot is too high" in {
    forAll { (currentBlockId: TypedIdentifier, transactionWithRandomTime: Transaction) =>
      val transaction =
        transactionWithRandomTime.copy(chronology =
          transactionWithRandomTime.chronology.copy(maximumSlot = Long.MaxValue)
        )
      val deferred = Deferred[F, Unit].unsafeRunSync()
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
        .expects(100L)
        .once()
        .returning(deferred.get)
      val underTest =
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
          .unsafeRunSync()

      underTest.add(transaction.id).unsafeRunSync()
      underTest.read(currentBlockId).unsafeRunSync() shouldBe Set(transaction.id.asTypedBytes)
      deferred.complete(()).unsafeRunSync()
      retry(underTest.read(currentBlockId).map(_ shouldBe Set.empty[TypedIdentifier])).unsafeRunSync()
    }
  }

  it should "expire a transaction more aggressively when a double-spend is detected" in {
    forAll {
      (
        baseTransaction: Transaction,
        input:           Transaction.Input,
        ancestorBlockId: TypedIdentifier,
        blockIdA:        TypedIdentifier,
        blockIdB:        TypedIdentifier
      ) =>
        val transactionWithSingleInput = baseTransaction.copy(inputs = Chain(input))
        // Transaction A and Transaction B are exactly the same, except for the creation timestamp to force a different ID
        val transactionA =
          transactionWithSingleInput.copy(chronology = transactionWithSingleInput.chronology.copy(creation = 0))
        val transactionAId = transactionA.id.asTypedBytes
        val transactionB =
          transactionWithSingleInput.copy(chronology = transactionWithSingleInput.chronology.copy(creation = 1))
        val transactionBId = transactionB.id.asTypedBytes
        val bodies =
          Map(
            blockIdA -> List(transactionAId),
            blockIdB -> List(transactionBId)
          )
        val transactions =
          Map(
            transactionAId -> transactionA,
            transactionBId -> transactionB
          )

        val fetchBody = (id: TypedIdentifier) => bodies(id).pure[F]
        val fetchTransaction = (id: TypedIdentifier) => transactions(id).pure[F]
        val tree = ParentChildTree.FromSemaphore.make[F, TypedIdentifier].unsafeRunSync()
        tree.associate(blockIdA, ancestorBlockId).unsafeRunSync()
        tree.associate(blockIdB, ancestorBlockId).unsafeRunSync()
        val clock = mock[ClockAlgebra[F]]
        (() => clock.globalSlot)
          .expects()
          .anyNumberOfTimes()
          .returning(0L.pure[F])
        val underTest =
          Mempool
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
            .unsafeRunSync()

        underTest.read(blockIdA).unsafeRunSync() shouldBe Set.empty[TypedIdentifier]

        inSequence {
          // The "unapply" operation will schedule a normal expiration
          (clock
            .delayedUntilSlot(_: Slot))
            .expects(transactionA.chronology.maximumSlot)
            .once()
            .returning(MonadCancel[F].never[Unit])
          // But the "apply" operation will re-schedule the expiration
          (clock
            .delayedUntilSlot(_: Slot))
            .expects(1L)
            .once()
            .returning(MonadCancel[F].never[Unit])
        }
        underTest.read(blockIdB).unsafeRunSync() shouldBe Set(transactionAId)
    }
  }

  private def retry[A](f: => F[A], attempts: Int = 5, delay: FiniteDuration = 100.milli): F[A] =
    if (attempts > 1)
      MonadThrow[F].recoverWith(f) { case _ => IO.sleep(delay) >> retry(f, attempts - 1, delay) }
    else
      f

}
