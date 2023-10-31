package co.topl.ledger.interpreters

import cats.effect.IO
import cats.implicits._
import co.topl.brambl.constants.NetworkConstants
import co.topl.brambl.generators.ModelGenerators._
import co.topl.brambl.models.{Datum, LockAddress, TransactionId}
import co.topl.brambl.models.box.{Lock, Value}
import co.topl.brambl.models.transaction.{IoTransaction, SpentTransactionOutput, UnspentTransactionOutput}
import co.topl.brambl.syntax._
import co.topl.models.generators.consensus.ModelGenerators._
import co.topl.models.ModelGenerators._
import co.topl.numerics.implicits._
import co.topl.consensus.models.{BlockId, StakingAddress}
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

  test("validation should fail if a duplicate staking address is found") {
    withMock {
      val parentBlockId = arbitraryBlockId.arbitrary.first
      val lock: Lock = Lock().withPredicate(Lock.Predicate())
      val lockAddress: LockAddress =
        lock.lockAddress(NetworkConstants.PRIVATE_NETWORK_ID, NetworkConstants.MAIN_LEDGER_ID)
      val registration0 = arbitraryStakingRegistration.arbitrary.first
      val tx0 =
        IoTransaction(datum = Datum.IoTransaction.defaultInstance)
          .withOutputs(
            List(UnspentTransactionOutput(lockAddress, Value().withTopl(Value.TOPL(5, registration0.some))))
          )
          .embedId
      val fetchTransaction = mockFunction[TransactionId, F[IoTransaction]]
      fetchTransaction
        .expects(tx0.id)
        .anyNumberOfTimes()
        .returning(tx0.pure[F])
      val transactionSemanticValidation = mock[TransactionSemanticValidationAlgebra[F]]
      (transactionSemanticValidation
        .validate(_: TransactionValidationContext)(_: IoTransaction))
        .expects(*, *)
        .once()
        .returning(tx0.validNec[TransactionSemanticError].pure[F])
      val registrationAccumulator = mock[RegistrationAccumulatorAlgebra[F]]
      (registrationAccumulator
        .contains(_: BlockId)(_: StakingAddress))
        .expects(parentBlockId, registration0.address)
        .once()
        .returning(true.pure[F])

      val body = BlockBody(List(tx0.id))
      for {
        underTest <- BodySemanticValidation
          .make[F](fetchTransaction, transactionSemanticValidation, registrationAccumulator)
        result <- underTest.validate(StaticBodyValidationContext(parentBlockId, 5, 10))(body)
        _      <- IO(result.isInvalid).assert
      } yield ()
    }
  }

  test("validation should fail if a the reward transaction does not spend the parent header ID") {
    withMock {
      val parentBlockId = arbitraryBlockId.arbitrary.first

      val rewardTx = IoTransaction.defaultInstance
        .withInputs(
          List(
            SpentTransactionOutput(
              arbitraryTransactionOutputAddress.arbitrary.first,
              arbitraryAttestation.arbitrary.first,
              Value().withLvl(Value.LVL(100L))
            )
          )
        )
        .withOutputs(
          List(
            UnspentTransactionOutput(
              arbitraryLockAddress.arbitrary.first,
              Value().withLvl(Value.LVL(100L))
            )
          )
        )
        .embedId

      val fetchTransaction = mockFunction[TransactionId, F[IoTransaction]]
      val transactionSemanticValidation = mock[TransactionSemanticValidationAlgebra[F]]
      val registrationAccumulator = mock[RegistrationAccumulatorAlgebra[F]]

      fetchTransaction
        .expects(rewardTx.id)
        .once()
        .returning(rewardTx.pure[F])

      // Note: Body Syntax Validation would ordinarily prohibit the case of a reward transaction for an empty body
      val body = BlockBody(Nil, rewardTx.id.some)
      for {
        underTest <- BodySemanticValidation
          .make[F](fetchTransaction, transactionSemanticValidation, registrationAccumulator)
        result <- underTest.validate(StaticBodyValidationContext(parentBlockId, 5, 10))(body)
        _      <- IO(result.isInvalid).assert
      } yield ()
    }
  }
}
