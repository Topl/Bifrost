package co.topl.ledger.interpreters

import cats.data.Chain
import cats.effect.IO
import cats.implicits._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.ledger.algebras._
import co.topl.ledger.models._
import co.topl.models.ModelGenerators._
import co.topl.models._
import co.topl.typeclasses.implicits._
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory

class TransactionSemanticValidationSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {
  type F[A] = IO[A]

  test("Spending out-of-bounds index should fail") {
    PropF.forAllF {
      (blockId: TypedIdentifier, transactionA: Transaction, _transactionB: Transaction, input: Transaction.Input) =>
        withMock {
          val transactionB = _transactionB.copy(inputs =
            Chain(
              input.copy(
                boxId = Box.Id(transactionA.id, Short.MaxValue)
              )
            )
          )
          for {
            fetchTransaction <- mockFunction[TypedIdentifier, F[Transaction]].pure[F]
            boxState         <- mock[BoxStateAlgebra[F]].pure[F]
            _ = fetchTransaction.expects(transactionA.id.asTypedBytes).once().returning(transactionA.pure[F])
            _ = (boxState
              .boxExistsAt(_: TypedIdentifier)(_: Box.Id))
              .expects(*, *)
              .anyNumberOfTimes()
              .returning(true.pure[F])
            underTest <- TransactionSemanticValidation.make[F](fetchTransaction, boxState)
            _ <- underTest
              .validate(blockId)(transactionB)
              .map(_.toEither.swap.getOrElse(???).exists(_.isInstanceOf[TransactionSemanticErrors.UnspendableBox]))
              .assert
          } yield ()
        }
    }
  }

  test("Box value mismatch should fail") {
    PropF.forAllF {
      (
        blockId:       TypedIdentifier,
        _transactionA: Transaction,
        _transactionB: Transaction,
        output:        Transaction.Output,
        input:         Transaction.Input
      ) =>
        withMock {
          val transactionA = _transactionA.copy(
            outputs = Chain(
              output.copy(
                address = output.address.copy(spendingAddress = input.proposition.spendingAddress)
              )
            )
          )
          val transactionB = _transactionB.copy(inputs =
            Chain(
              input.copy(
                boxId = Box.Id(transactionA.id, 0)
              )
            )
          )
          for {
            fetchTransaction <- mockFunction[TypedIdentifier, F[Transaction]].pure[F]
            boxState         <- mock[BoxStateAlgebra[F]].pure[F]
            _ = fetchTransaction.expects(transactionA.id.asTypedBytes).once().returning(transactionA.pure[F])
            _ = (boxState
              .boxExistsAt(_: TypedIdentifier)(_: Box.Id))
              .expects(*, *)
              .anyNumberOfTimes()
              .returning(true.pure[F])
            underTest <- TransactionSemanticValidation.make[F](fetchTransaction, boxState)
            result <- underTest
              .validate(blockId)(transactionB)
            _ <- IO(
              if (transactionA.outputs.headOption.get.value =!= transactionB.inputs.headOption.get.value)
                result.toEither.swap.getOrElse(???).exists(_.isInstanceOf[TransactionSemanticErrors.InputDataMismatch])
              else
                true
            ).assert
          } yield ()
        }
    }
  }

  test("Address -> Proposition mismatch should fail") {
    PropF.forAllF {
      (
        blockId:       TypedIdentifier,
        _transactionA: Transaction,
        _transactionB: Transaction,
        output:        Transaction.Output,
        input:         Transaction.Input
      ) =>
        withMock {
          val transactionA = _transactionA.copy(
            outputs = Chain(
              output.copy(value = Box.Values.Empty)
            )
          )
          val transactionB = _transactionB.copy(inputs =
            Chain(
              input.copy(
                value = Box.Values.Empty,
                boxId = Box.Id(transactionA.id, 0)
              )
            )
          )
          for {
            fetchTransaction <- mockFunction[TypedIdentifier, F[Transaction]].pure[F]
            boxState         <- mock[BoxStateAlgebra[F]].pure[F]
            _ = fetchTransaction.expects(transactionA.id.asTypedBytes).once().returning(transactionA.pure[F])
            _ = (boxState
              .boxExistsAt(_: TypedIdentifier)(_: Box.Id))
              .expects(*, *)
              .anyNumberOfTimes()
              .returning(true.pure[F])
            underTest <- TransactionSemanticValidation.make[F](fetchTransaction, boxState)
            result    <- underTest.validate(blockId)(transactionB)
            _ <- IO(
              result.toEither.swap.getOrElse(???).exists(_.isInstanceOf[TransactionSemanticErrors.InputDataMismatch])
            ).assert
          } yield ()
        }
    }
  }

  test("Double spends should fail") {
    PropF.forAllF {
      (
        blockId:       TypedIdentifier,
        _transactionA: Transaction,
        _transactionB: Transaction,
        output:        Transaction.Output,
        input:         Transaction.Input
      ) =>
        withMock {
          val transactionA = _transactionA.copy(
            outputs = Chain(
              output.copy(
                value = Box.Values.Empty,
                address = output.address.copy(spendingAddress = input.proposition.spendingAddress)
              )
            )
          )
          val transactionB = _transactionB.copy(inputs =
            Chain(
              input.copy(
                value = Box.Values.Empty,
                boxId = Box.Id(transactionA.id, 0)
              )
            )
          )
          for {
            fetchTransaction <- mockFunction[TypedIdentifier, F[Transaction]].pure[F]
            boxState         <- mock[BoxStateAlgebra[F]].pure[F]
            _ = fetchTransaction.expects(transactionA.id.asTypedBytes).once().returning(transactionA.pure[F])
            _ = (boxState
              .boxExistsAt(_: TypedIdentifier)(_: Box.Id))
              .expects(blockId, Box.Id(transactionA.id, 0))
              .once()
              .returning(false.pure[F])
            underTest <- TransactionSemanticValidation.make[F](fetchTransaction, boxState)
            result    <- underTest.validate(blockId)(transactionB)
            _ <- IO(
              result.toEither.swap.getOrElse(???).exists(_.isInstanceOf[TransactionSemanticErrors.UnspendableBox])
            ).assert
          } yield ()
        }
    }
  }
}
