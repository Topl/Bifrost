package co.topl.ledger.interpreters

import cats.data.Chain
import cats.effect.IO
import cats.implicits._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.ledger.algebras.{BoxStateAlgebra, TransactionSemanticValidationAlgebra}
import co.topl.ledger.models._
import co.topl.models.ModelGenerators._
import co.topl.models.{Box, Transaction, TypedIdentifier}
import co.topl.typeclasses.implicits._
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory

import scala.collection.immutable.ListSet

class BodySemanticValidationSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  test("validation should fail if any transaction is semantically invalid") {
    PropF.forAllF { (parentBlockId: TypedIdentifier, _transaction: Transaction, input: Transaction.Input) =>
      val transaction = _transaction.copy(inputs = Chain(input))
      withMock {
        val body = ListSet(transaction.id.asTypedBytes)
        for {
          fetchTransaction <- mockFunction[TypedIdentifier, F[Transaction]].pure[F]
          _ = fetchTransaction.expects(transaction.id.asTypedBytes).once().returning(transaction.pure[F])
          transactionSemanticValidation = mock[TransactionSemanticValidationAlgebra[F]]
          _ = (transactionSemanticValidation
            .validate(_: TypedIdentifier)(_: Transaction))
            .expects(parentBlockId, transaction)
            .once()
            .returning(
              (TransactionSemanticErrors
                .InputDataMismatch(input): TransactionSemanticError).invalidNec[Transaction].pure[F]
            )
          boxState = mock[BoxStateAlgebra[F]]
          makeTransactionSemanticValidation = (_: BoxStateAlgebra[F]) => transactionSemanticValidation.pure[F]
          underTest <- BodySemanticValidation.make[F](fetchTransaction, boxState, makeTransactionSemanticValidation)
          result    <- underTest.validate(parentBlockId)(body)
          _         <- IO(result.isInvalid).assert
        } yield ()
      }
    }
  }

  test("validation should fail if two transactions within the block spend the same box") {
    PropF.forAllF {
      (
        parentBlockId: TypedIdentifier,
        _transactionA: Transaction,
        _transactionB: Transaction,
        input:         Transaction.Input
      ) =>
        val transactionA = _transactionA.copy(inputs = Chain(input))
        val transactionB = _transactionB.copy(inputs = Chain(input))
        withMock {
          val body = ListSet(transactionA.id.asTypedBytes, transactionB.id.asTypedBytes)
          for {
            fetchTransaction <- mockFunction[TypedIdentifier, F[Transaction]].pure[F]
            _ = fetchTransaction
              .expects(transactionA.id.asTypedBytes)
              .anyNumberOfTimes()
              .returning(transactionA.pure[F])
            _ = fetchTransaction
              .expects(transactionB.id.asTypedBytes)
              .anyNumberOfTimes()
              .returning(transactionB.pure[F])
            transactionSemanticValidation = mock[TransactionSemanticValidationAlgebra[F]]
            _ = (transactionSemanticValidation
              .validate(_: TypedIdentifier)(_: Transaction))
              .expects(parentBlockId, transactionA)
              .once()
              .returning(
                (TransactionSemanticErrors
                  .InputDataMismatch(input): TransactionSemanticError).invalidNec[Transaction].pure[F]
              )
            _ = (transactionSemanticValidation
              .validate(_: TypedIdentifier)(_: Transaction))
              .expects(parentBlockId, transactionB)
              .never() // TransactionB should fail before reaching transaction semantic validation
            boxState = mock[BoxStateAlgebra[F]]
            makeTransactionSemanticValidation = mockFunction[BoxStateAlgebra[F], F[
              TransactionSemanticValidationAlgebra[F]
            ]]
            _ <- inSequence {
              var firstTransactionValidated = false
              makeTransactionSemanticValidation.expects(*).once().onCall { boxState1: BoxStateAlgebra[F] =>
                (if (!firstTransactionValidated)
                   boxState1.boxExistsAt(parentBlockId)(input.boxId).assert
                 else {
                   firstTransactionValidated = true
                   boxState1.boxExistsAt(parentBlockId)(input.boxId).map(!_).assert
                 }) >>
                transactionSemanticValidation.pure[F]
              }
              (boxState
                .boxExistsAt(_: TypedIdentifier)(_: Box.Id))
                .expects(parentBlockId, input.boxId)
                .once()
                .returning(true.pure[F])
              for {
                underTest <- BodySemanticValidation
                  .make[F](fetchTransaction, boxState, makeTransactionSemanticValidation)
                result <- underTest.validate(parentBlockId)(body)
                _      <- IO(result.isInvalid).assert
              } yield ()
            }
          } yield ()
        }
    }
  }
}
