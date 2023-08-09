package co.topl.ledger.interpreters

import cats.effect.IO
import cats.implicits._
import co.topl.brambl.constants.NetworkConstants
import co.topl.brambl.generators.ModelGenerators._
import co.topl.brambl.models._
import co.topl.brambl.models.box.{FixedSeries, Value}
import co.topl.brambl.models.transaction._
import co.topl.brambl.syntax._
import co.topl.consensus.models.BlockId
import co.topl.models.generators.consensus.ModelGenerators._
import co.topl.ledger.algebras._
import co.topl.ledger.models._
import com.google.protobuf.ByteString
import munit.CatsEffectSuite
import munit.ScalaCheckEffectSuite
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory
import quivr.models.Int128

class TransactionSemanticValidationSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {
  type F[A] = IO[A]

  test("Invalid schedule+slot combination should fail") {
    PropF.forAllF {
      (blockId: BlockId, transactionA: IoTransaction, _transactionB: IoTransaction, input: SpentTransactionOutput) =>
        withMock {
          val transactionB = _transactionB.clearInputs
            .addInputs(
              input.copy(address = transactionA.id.outputAddress(0, 0, 0))
            )
            .update(_.datum.event.schedule.min.set(0))
            .update(_.datum.event.schedule.max.set(4))
          for {
            fetchTransaction <- mockFunction[TransactionId, F[IoTransaction]].pure[F]
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
          val transactionB = _transactionB.clearInputs.addInputs(
            input.copy(transactionA.id.outputAddress(0, 0, Short.MaxValue))
          )
          for {
            fetchTransaction <- mockFunction[TransactionId, F[IoTransaction]].pure[F]
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
          val lockAddress = input.attestation.getPredicate.lock.lockAddress(
            NetworkConstants.PRIVATE_NETWORK_ID,
            NetworkConstants.MAIN_LEDGER_ID
          )
          val transactionA = _transactionA.update(
            _.outputs.set(
              List(
                output.update(_.address.set(lockAddress))
              )
            )
          )
          val transactionB = _transactionB.update(
            _.inputs.set(
              List(
                input.update(
                  _.address.set(transactionA.id.outputAddress(0, 0, 0))
                )
              )
            )
          )
          for {
            fetchTransaction <- mockFunction[TransactionId, F[IoTransaction]].pure[F]
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
          val transactionA = _transactionA.copy(outputs = List(output.copy(value = Value.defaultInstance)))
          val transactionB = _transactionB.copy(inputs =
            List(
              input.copy(
                value = Value.defaultInstance,
                address = transactionA.id.outputAddress(0, 0, 0)
              )
            )
          )
          for {
            fetchTransaction <- mockFunction[TransactionId, F[IoTransaction]].pure[F]
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
            outputs = List(
              output.copy(
                value = Value.defaultInstance,
                address = input.attestation.getPredicate.lock.lockAddress(
                  NetworkConstants.PRIVATE_NETWORK_ID,
                  NetworkConstants.MAIN_LEDGER_ID
                )
              )
            )
          )
          val transactionB = _transactionB.copy(inputs =
            List(
              input.copy(
                value = Value.defaultInstance,
                address = transactionA.id.outputAddress(0, 0, 0)
              )
            )
          )
          for {
            fetchTransaction <- mockFunction[TransactionId, F[IoTransaction]].pure[F]
            boxState         <- mock[BoxStateAlgebra[F]].pure[F]
            _ = fetchTransaction.expects(transactionA.id).once().returning(transactionA.pure[F])
            _ = (boxState
              .boxExistsAt(_: BlockId)(_: TransactionOutputAddress))
              .expects(blockId, transactionA.id.outputAddress(0, 0, 0))
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

  test("Invalid outputs on group constructor token should fail") {
    PropF.forAllF {
      (blockId: BlockId, _transactionA: IoTransaction, _transactionB: IoTransaction, input: SpentTransactionOutput) =>
        withMock {
          val transactionA = _transactionA.copy(
            outputs = List(
              UnspentTransactionOutput(
                address = input.attestation.getPredicate.lock.lockAddress(
                  NetworkConstants.PRIVATE_NETWORK_ID,
                  NetworkConstants.MAIN_LEDGER_ID
                ),
                Value.defaultInstance.withTopl(
                  Value.TOPL(
                    quantity = Int128.defaultInstance.withValue(ByteString.copyFrom(BigInt(1).toByteArray))
                  )
                )
              )
            )
          )
          val transactionB = _transactionB.clearInputs.clearOutputs
            .addInputs(
              input.copy(
                address = transactionA.id.outputAddress(0, 0, 0),
                value = transactionA.outputs(0).value
              )
            )
            .addOutputs(
              UnspentTransactionOutput(
                input.attestation.getPredicate.lock.lockAddress(0, 0),
                Value.defaultInstance.withGroup(
                  Value.Group(
                    label = "Crypto Frogs",
                    fixedSeries = Option.empty[FixedSeries],
                    txId = transactionA.id,
                    index = 0, // it is a TOPL values instead of LVL
                    groupId = Option.empty[GroupId]
                  )
                )
              )
            )

          for {
            fetchTransaction <- mockFunction[TransactionId, F[IoTransaction]].pure[F]
            boxState         <- mock[BoxStateAlgebra[F]].pure[F]
            _ = fetchTransaction.expects(transactionA.id).twice().returning(transactionA.pure[F])
            underTest <- TransactionSemanticValidation.make[F](fetchTransaction, boxState)
            _ <- underTest
              .validate(
                StaticTransactionValidationContext(blockId, Seq(transactionA), 1, transactionB.datum.event.schedule.min)
              )(transactionB)
              .map(
                _.toEither.swap
                  .getOrElse(???)
                  .exists(_.isInstanceOf[TransactionSemanticErrors.UnsatisfiedGroupMismatch])
              )
              .assert
          } yield ()
        }
    }
  }
}
