package co.topl.ledger.interpreters

import cats.data.Chain
import cats.effect.IO
import cats.implicits._
import co.topl.brambl.models.Identifier
import co.topl.brambl.models.TransactionOutputAddress
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.models.transaction.SpentTransactionOutput
import co.topl.brambl.models.transaction.UnspentTransactionOutput
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.consensus.models.BlockId
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

  test("Invalid schedule+slot combination should fail") {
    PropF.forAllF {
      (blockId: BlockId, transactionA: IoTransaction, _transactionB: IoTransaction, input: SpentTransactionOutput) =>
        withMock {
          val transactionB = _transactionB.clearInputs
            .addInputs(
              input.copy(address =
                TransactionOutputAddress(0, 0, 0, TransactionOutputAddress.Id.IoTransaction32(transactionA.id))
              )
            )
            .update(_.datum.event.schedule.min.set(0))
            .update(_.datum.event.schedule.max.set(4))
          for {
            fetchTransaction <- mockFunction[Identifier.IoTransaction32, F[Transaction]].pure[F]
            boxState         <- mock[BoxStateAlgebra[F]].pure[F]
            underTest        <- TransactionSemanticValidation.make[F](fetchTransaction, boxState)
            _ <- underTest
              .validate(StaticTransactionValidationContext(blockId, Nil, 1, 5))(transactionB)
              .map(_.toEither.swap.getOrElse(???).exists(_.isInstanceOf[TransactionSemanticErrors.UnsatisfiedSchedule]))
              .assert
          } yield ()
        }
    }
  }

  test("Spending out-of-bounds index should fail") {
    PropF.forAllF {
      (blockId: BlockId, transactionA: IoTransaction, _transactionB: IoTransaction, input: SpentTransactionOutput) =>
        withMock {
          val transactionB = _transactionB.clearInputs.addInputs(input.copy(
            TransactionOutputAddress(0,0,Short.MaxValue, TransactionOutputAddress.Id.IoTransaction32(transactionA.id))
          ))
          for {
            fetchTransaction <- mockFunction[Identifier.IoTransaction32, F[IoTransaction]].pure[F]
            boxState         <- mock[BoxStateAlgebra[F]].pure[F]
            _ = fetchTransaction.expects(transactionA.id).once().returning(transactionA.pure[F])
            _ = (boxState
              .boxExistsAt(_: BlockId)(_: TransactionOutputAddress))
              .expects(*, *)
              .anyNumberOfTimes()
              .returning(true.pure[F])
            underTest <- TransactionSemanticValidation.make[F](fetchTransaction, boxState)
            _ <- underTest
              .validate(StaticTransactionValidationContext(blockId, Nil, 1, transactionB.datum.event.schedule.min))(
                transactionB
              )
              .map(_.toEither.swap.getOrElse(???).exists(_.isInstanceOf[TransactionSemanticErrors.UnspendableBox]))
              .assert
          } yield ()
        }
    }
  }

  test("Box value mismatch should fail") {
    PropF.forAllF {
      (
        blockId:       BlockId,
        _transactionA: IoTransaction,
        _transactionB: IoTransaction,
        output:        UnspentTransactionOutput,
        input:         SpentTransactionOutput
      ) =>
        withMock {
          val transactionA = _transactionA.copy(
            outputs = Chain(
              output.copy(
                address = output.address.copy(address = input.attestation.getPredicate.lock.address)
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
            fetchTransaction <- mockFunction[Identifier.IoTransaction32, F[IoTransaction]].pure[F]
            boxState         <- mock[BoxStateAlgebra[F]].pure[F]
            _ = fetchTransaction.expects(transactionA.id).once().returning(transactionA.pure[F])
            _ = (boxState
              .boxExistsAt(_: BlockId)(_: TransactionOutputAddress))
              .expects(*, *)
              .anyNumberOfTimes()
              .returning(true.pure[F])
            underTest <- TransactionSemanticValidation.make[F](fetchTransaction, boxState)
            result <- underTest
              .validate(StaticTransactionValidationContext(blockId, Nil, 1, transactionB.datum.event.schedule.min))(
                transactionB
              )
            _ <- IO(
              if (transactionA.outputs.headOption.get.value != transactionB.inputs.headOption.get.value)
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
        blockId:       BlockId,
        _transactionA: IoTransaction,
        _transactionB: IoTransaction,
        output:        UnspentTransactionOutput,
        input:         SpentTransactionOutput
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
            fetchTransaction <- mockFunction[BlockId, F[IoTransaction]].pure[F]
            boxState         <- mock[BoxStateAlgebra[F]].pure[F]
            _ = fetchTransaction.expects(transactionA.id).once().returning(transactionA.pure[F])
            _ = (boxState
              .boxExistsAt(_: BlockId)(_: TransactionOutputAddress))
              .expects(*, *)
              .anyNumberOfTimes()
              .returning(true.pure[F])
            underTest <- TransactionSemanticValidation.make[F](fetchTransaction, boxState)
            result <- underTest.validate(
              StaticTransactionValidationContext(blockId, Nil, 1, transactionB.datum.event.schedule.min)
            )(transactionB)
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
        blockId:       BlockId,
        _transactionA: IoTransaction,
        _transactionB: IoTransaction,
        output:        UnspentTransactionOutput,
        input:         SpentTransactionOutput
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
            fetchTransaction <- mockFunction[Identifier.IoTransaction32, F[IoTransaction]].pure[F]
            boxState         <- mock[BoxStateAlgebra[F]].pure[F]
            _ = fetchTransaction.expects(transactionA.id).once().returning(transactionA.pure[F])
            _ = (boxState
              .boxExistsAt(_: BlockId)(_: TransactionOutputAddress))
              .expects(blockId, Box.Id(transactionA.id, 0))
              .once()
              .returning(false.pure[F])
            underTest <- TransactionSemanticValidation.make[F](fetchTransaction, boxState)
            result <- underTest.validate(
              StaticTransactionValidationContext(blockId, Nil, 1, transactionB.datum.event.schedule.min)
            )(transactionB)
            _ <- IO(
              result.toEither.swap.getOrElse(???).exists(_.isInstanceOf[TransactionSemanticErrors.UnspendableBox])
            ).assert
          } yield ()
        }
    }
  }
}
