package co.topl.ledger.interpreters

import cats.effect.IO
import cats.implicits._
import co.topl.brambl.generators.ModelGenerators._
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.models.transaction.SpentTransactionOutput
import co.topl.brambl.syntax._
import co.topl.models.generators.consensus.ModelGenerators._
import co.topl.consensus.models.BlockId
import co.topl.ledger.algebras.{RegistrationAccumulatorAlgebra, TransactionSemanticValidationAlgebra}
import co.topl.ledger.models._
import co.topl.node.models.BlockBody
import munit.CatsEffectSuite
import munit.ScalaCheckEffectSuite
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory

class BodySemanticValidationSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  test("validation should fail if any transaction is semantically invalid") {
    PropF.forAllF {
      (parentBlockId: BlockId, _transaction: IoTransaction, input: SpentTransactionOutput, height: Long, slot: Long) =>
        val transaction = _transaction.addInputs(input)
        withMock {
          val body = BlockBody(List(transaction.id))
          for {
            fetchTransaction <- mockFunction[TransactionId, F[IoTransaction]].pure[F]
            _ = fetchTransaction.expects(transaction.id).once().returning(transaction.pure[F])
            transactionSemanticValidation = mock[TransactionSemanticValidationAlgebra[F]]
            _ = (transactionSemanticValidation
              .validate(_: TransactionValidationContext)(_: IoTransaction))
              .expects(StaticTransactionValidationContext(parentBlockId, Nil, height, slot), transaction)
              .once()
              .returning(
                (TransactionSemanticErrors
                  .InputDataMismatch(input): TransactionSemanticError).invalidNec[IoTransaction].pure[F]
              )
            registrationAccumulator = mock[RegistrationAccumulatorAlgebra[F]]
            underTest <- BodySemanticValidation
              .make[F](fetchTransaction, transactionSemanticValidation, registrationAccumulator)
            result <- underTest.validate(StaticBodyValidationContext(parentBlockId, height, slot))(body)
            _      <- IO(result.isInvalid).assert
          } yield ()
        }
    }
  }

  test("validation should fail if two transactions within the block spend the same box") {
    PropF.forAllF {
      (
        parentBlockId: BlockId,
        _transactionA: IoTransaction,
        _transactionB: IoTransaction,
        input:         SpentTransactionOutput,
        height:        Long,
        slot:          Long
      ) =>
        val transactionA = _transactionA.addInputs(input)
        val transactionB = _transactionB.addInputs(input)
        withMock {
          val body = BlockBody(List(transactionA.id, transactionB.id))
          for {
            fetchTransaction <- mockFunction[TransactionId, F[IoTransaction]].pure[F]
            _ = fetchTransaction
              .expects(transactionA.id)
              .anyNumberOfTimes()
              .returning(transactionA.pure[F])
            _ = fetchTransaction
              .expects(transactionB.id)
              .anyNumberOfTimes()
              .returning(transactionB.pure[F])
            transactionSemanticValidation = mock[TransactionSemanticValidationAlgebra[F]]
            _ = (transactionSemanticValidation
              .validate(_: TransactionValidationContext)(_: IoTransaction))
              .expects(StaticTransactionValidationContext(parentBlockId, Nil, height, slot), transactionA)
              .once()
              .returning(
                (TransactionSemanticErrors
                  .InputDataMismatch(input): TransactionSemanticError).invalidNec[IoTransaction].pure[F]
              )
            _ = (transactionSemanticValidation
              .validate(_: TransactionValidationContext)(_: IoTransaction))
              .expects(*, *)
              .never() // TransactionB should fail before reaching transaction semantic validation
            registrationAccumulator = mock[RegistrationAccumulatorAlgebra[F]]
            underTest <- BodySemanticValidation
              .make[F](fetchTransaction, transactionSemanticValidation, registrationAccumulator)
            result <- underTest.validate(StaticBodyValidationContext(parentBlockId, height, slot))(body)
            _      <- IO(result.isInvalid).assert
          } yield ()
        }
    }
  }
}
