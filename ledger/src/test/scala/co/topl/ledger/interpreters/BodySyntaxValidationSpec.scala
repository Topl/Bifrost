package co.topl.ledger.interpreters

import cats.effect.IO
import cats.implicits._
import co.topl.brambl.constants.NetworkConstants
import co.topl.brambl.generators.ModelGenerators._
import co.topl.brambl.models.box.{Lock, Value}
import co.topl.brambl.models.transaction.{IoTransaction, SpentTransactionOutput, UnspentTransactionOutput}
import co.topl.brambl.models.{LockAddress, TransactionId}
import co.topl.brambl.syntax._
import co.topl.brambl.validation.TransactionSyntaxError
import co.topl.brambl.validation.algebras.TransactionSyntaxVerifier
import co.topl.ledger.algebras.TransactionRewardCalculatorAlgebra
import co.topl.ledger.models.BodySyntaxErrors
import co.topl.node.models.BlockBody
import co.topl.models.ModelGenerators._
import co.topl.numerics.implicits._
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory

class BodySyntaxValidationSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  test("validation should fail if any transaction is syntactically invalid") {
    PropF.forAllF { transaction: IoTransaction =>
      withMock {
        val body = BlockBody(List(transaction.id))
        for {
          fetchTransaction <- mockFunction[TransactionId, F[IoTransaction]].pure[F]
          _ = fetchTransaction.expects(transaction.id).once().returning(transaction.pure[F])
          transactionSyntaxValidation = mock[TransactionSyntaxVerifier[F]]
          _ = (transactionSyntaxValidation.validate _)
            .expects(transaction)
            .once()
            .returning(
              (TransactionSyntaxError.EmptyInputs: TransactionSyntaxError).invalidNec[IoTransaction].toEither.pure[F]
            )
          rewardCalculator = mock[TransactionRewardCalculatorAlgebra[F]]
          underTest <- BodySyntaxValidation.make[F](fetchTransaction, transactionSyntaxValidation, rewardCalculator)
          result    <- underTest.validate(body)
          _         <- IO(result.isInvalid).assert
        } yield ()
      }
    }
  }

  test("validation should fail if the reward is incorrect") {
    PropF.forAllF { transaction: IoTransaction =>
      withMock {
        val lock: Lock = Lock().withPredicate(Lock.Predicate())

        val lockAddress: LockAddress =
          lock.lockAddress(NetworkConstants.PRIVATE_NETWORK_ID, NetworkConstants.MAIN_LEDGER_ID)
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
                lockAddress,
                Value().withLvl(Value.LVL(100L))
              )
            )
          )
          .embedId
        val body = BlockBody(List(transaction.id), rewardTx.id.some)
        for {
          fetchTransaction <- mockFunction[TransactionId, F[IoTransaction]].pure[F]
          _ = fetchTransaction.expects(transaction.id).once().returning(transaction.pure[F])
          _ = fetchTransaction.expects(rewardTx.id).once().returning(rewardTx.pure[F])
          transactionSyntaxValidation = mock[TransactionSyntaxVerifier[F]]
          _ = (transactionSyntaxValidation.validate _)
            .expects(transaction)
            .once()
            .returning(transaction.validNec[TransactionSyntaxError].toEither.pure[F])
          rewardCalculator = mock[TransactionRewardCalculatorAlgebra[F]]
          _ = (rewardCalculator
            .rewardOf(_))
            .expects(transaction)
            .once()
            .returning(BigInt(2L).pure[F])
          underTest <- BodySyntaxValidation.make[F](fetchTransaction, transactionSyntaxValidation, rewardCalculator)
          result    <- underTest.validate(body)
          _         <- IO(result.isInvalid).assert
          _         <- IO(result.swap.toOption.get.toList == List(BodySyntaxErrors.InvalidReward(rewardTx))).assert
        } yield ()
      }
    }
  }
}
