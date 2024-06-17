package co.topl.minting.interpreters

import cats.effect.{IO, Resource}
import co.topl.algebras.Stats.Implicits._
import cats.implicits._
import co.topl.algebras.ContextlessValidationAlgebra
import co.topl.brambl.generators.ModelGenerators._
import co.topl.brambl.models._
import co.topl.brambl.models.box.{Attestation, Value}
import co.topl.brambl.models.transaction.{IoTransaction, SpentTransactionOutput, UnspentTransactionOutput}
import co.topl.brambl.syntax._
import co.topl.brambl.validation.TransactionAuthorizationError
import co.topl.brambl.validation.algebras._
import co.topl.consensus.models.BlockId
import co.topl.ledger.algebras._
import co.topl.ledger.models.{MempoolGraph, RewardQuantities, TransactionSemanticError, TransactionSemanticErrors}
import co.topl.models.ModelGenerators._
import co.topl.models.Slot
import co.topl.models.generators.consensus.ModelGenerators
import co.topl.quivr.runtime.DynamicContext
import com.google.protobuf.ByteString
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory
import quivr.models.Int128

import java.util.concurrent.TimeoutException
import scala.concurrent.duration._

class BlockPackerSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {
  type F[A] = IO[A]

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMaxSize(3)
      .withMinSuccessfulTests(5)

  private val dummyRewardCalc: TransactionRewardCalculatorAlgebra = (_: IoTransaction) => RewardQuantities()
  private val dummyCostCalc: TransactionCostCalculator = (tx: IoTransaction) => tx.inputs.size

  test("return empty for empty mempool") {
    withMock {
      val mempool = mock[MempoolAlgebra[F]]
      (mempool
        .read(_: BlockId))
        .expects(*)
        .anyNumberOfTimes()
        .returning(MempoolGraph.empty[F](dummyRewardCalc, dummyCostCalc).pure[F])
      val testResource =
        for {
          underTest <- BlockPacker.make[F](
            mempool,
            mock[BoxStateAlgebra[F]],
            mock[TransactionRewardCalculatorAlgebra],
            mock[TransactionCostCalculator],
            mock[BlockPackerValidation[F]],
            mock[RegistrationAccumulatorAlgebra[F]]
          )
          _ <- underTest
            .blockImprover(ModelGenerators.arbitraryBlockId.arbitrary.first, 0, 0)
            // Timeout should be less than the Block Packer's mempool polling period
            .timeout(200.milli)
            .compile
            .toList
            .intercept[TimeoutException]
            .toResource
        } yield ()

      testResource.use_
    }
  }

  test("return a valid block for a simple mempool") {
    withMock {
      val tx1 =
        IoTransaction(datum = Datum.IoTransaction.defaultInstance)
          .withOutputs(
            List(
              UnspentTransactionOutput(value = lvlValue(100), address = emptyLockAddress),
              UnspentTransactionOutput(value = lvlValue(200), address = emptyLockAddress)
            )
          )
          .embedId

      val tx2 =
        IoTransaction(datum = Datum.IoTransaction.defaultInstance)
          .withInputs(
            List(stxo(tx1, 0))
          )
          .withOutputs(
            List(
              UnspentTransactionOutput(value = lvlValue(50), address = emptyLockAddress)
            )
          )
          .embedId

      val tx3 =
        IoTransaction(datum = Datum.IoTransaction.defaultInstance)
          .withInputs(
            List(stxo(tx1, 1))
          )
          .withOutputs(
            List(
              UnspentTransactionOutput(value = lvlValue(150), address = emptyLockAddress)
            )
          )
          .embedId

      val tx4 =
        IoTransaction(datum = Datum.IoTransaction.defaultInstance)
          .withInputs(
            List(
              stxo(tx2, 0),
              stxo(tx3, 0)
            )
          )
          .withOutputs(
            List(
              UnspentTransactionOutput(value = lvlValue(100), address = emptyLockAddress)
            )
          )
          .embedId

      val mempool = mock[MempoolAlgebra[F]]
      val mempoolGraph = MempoolGraph.empty[F](dummyRewardCalc, dummyCostCalc).add(tx2).add(tx3).add(tx4)
      (mempool.read(_: BlockId)).expects(*).once().returning(mempoolGraph.pure[F])
      val boxState = mock[BoxStateAlgebra[F]]
      (boxState
        .boxExistsAt(_: BlockId)(_: TransactionOutputAddress))
        .expects(*, tx2.inputs(0).address)
        .once()
        .returning(true.pure[F])
      (boxState
        .boxExistsAt(_: BlockId)(_: TransactionOutputAddress))
        .expects(*, tx3.inputs(0).address)
        .once()
        .returning(true.pure[F])
      val rewardCalculator = mock[TransactionRewardCalculatorAlgebra]
      (rewardCalculator
        .rewardsOf(_: IoTransaction))
        .expects(*)
        .anyNumberOfTimes()
        .returning(RewardQuantities(BigInt(100)))
      val costCalculator = mock[TransactionCostCalculator]
      (costCalculator.costOf(_: IoTransaction)).expects(*).anyNumberOfTimes().returning(50L)
      val validation = mock[BlockPackerValidation[F]]
      (validation
        .transactionIsValid(_: IoTransaction, _: Long, _: Slot))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .returns(true.pure[F])
      val registrationAccumulator = mock[RegistrationAccumulatorAlgebra[F]]
      val testResource =
        for {
          underTest <- BlockPacker.make[F](
            mempool,
            boxState,
            rewardCalculator,
            costCalculator,
            validation,
            registrationAccumulator
          )
          stream = underTest.blockImprover(ModelGenerators.arbitraryBlockId.arbitrary.first, 0, 0)
          result <- stream
            .interruptAfter(3.seconds)
            .compile
            .lastOrError
            .toResource
          _ <- IO(result.transactions).assertEquals(List(tx2, tx3, tx4)).toResource
        } yield ()

      testResource.use_
    }
  }

  test("evict transactions from the mempool when authorization fails") {
    withMock {
      val tx1 =
        IoTransaction(datum = Datum.IoTransaction.defaultInstance)
          .withOutputs(
            List(
              UnspentTransactionOutput(value = lvlValue(100), address = emptyLockAddress),
              UnspentTransactionOutput(value = lvlValue(200), address = emptyLockAddress)
            )
          )
          .embedId

      val tx2 =
        IoTransaction(datum = Datum.IoTransaction.defaultInstance)
          .withInputs(
            List(stxo(tx1, 0))
          )
          .withOutputs(
            List(
              UnspentTransactionOutput(value = lvlValue(50), address = emptyLockAddress)
            )
          )
          .embedId

      // tx3 will return invalid in the authorization interpreter mock, so it should be evicted from the mempool
      val tx3 =
        IoTransaction(datum = Datum.IoTransaction.defaultInstance)
          .withInputs(
            List(stxo(tx1, 1))
          )
          .withOutputs(
            List(
              UnspentTransactionOutput(value = lvlValue(150), address = emptyLockAddress)
            )
          )
          .embedId

      // tx4 is a child of tx3, so it will also be evicted
      val tx4 =
        IoTransaction(datum = Datum.IoTransaction.defaultInstance)
          .withInputs(
            List(
              stxo(tx2, 0),
              stxo(tx3, 0)
            )
          )
          .withOutputs(
            List(
              UnspentTransactionOutput(value = lvlValue(100), address = emptyLockAddress)
            )
          )
          .embedId

      val mempool = mock[MempoolAlgebra[F]]
      val mempoolGraph = MempoolGraph.empty[F](dummyRewardCalc, dummyCostCalc).add(tx2).add(tx3).add(tx4)
      (mempool.read(_: BlockId)).expects(*).once().returning(mempoolGraph.pure[F])
      (mempool.remove(_: TransactionId)).expects(tx3.id).once().returning(().pure[F])
      (mempool.remove(_: TransactionId)).expects(tx4.id).once().returning(().pure[F])
      val boxState = mock[BoxStateAlgebra[F]]
      (boxState
        .boxExistsAt(_: BlockId)(_: TransactionOutputAddress))
        .expects(*, tx2.inputs(0).address)
        .once()
        .returning(true.pure[F])
      val rewardCalculator = mock[TransactionRewardCalculatorAlgebra]
      (rewardCalculator
        .rewardsOf(_: IoTransaction))
        .expects(*)
        .anyNumberOfTimes()
        .returning(RewardQuantities(BigInt(100)))
      val costCalculator = mock[TransactionCostCalculator]
      (costCalculator.costOf(_: IoTransaction)).expects(*).anyNumberOfTimes().returning(50L)
      val validation = mock[BlockPackerValidation[F]]
      (validation
        .transactionIsValid(_: IoTransaction, _: Long, _: Slot))
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall { case (tx: IoTransaction, _: Long, _: Slot) =>
          if (tx.id == tx3.id) false.pure[F]
          else true.pure[F]
        }
      val registrationAccumulator = mock[RegistrationAccumulatorAlgebra[F]]
      val testResource =
        for {
          underTest <- BlockPacker.make[F](
            mempool,
            boxState,
            rewardCalculator,
            costCalculator,
            validation,
            registrationAccumulator
          )
          stream = underTest.blockImprover(ModelGenerators.arbitraryBlockId.arbitrary.first, 0, 0)
          result <- stream
            .interruptAfter(3.seconds)
            .compile
            .lastOrError
            .toResource
          _ <- IO(result.transactions).assertEquals(List(tx2)).toResource
        } yield ()

      testResource.use_
    }
  }

  test("validation") {
    withMock {
      val tx = IoTransaction(datum = Datum.IoTransaction.defaultInstance)

      // Test: Invalid Data
      val test1Resource =
        for {
          dataValidation <- Resource.pure[F, ContextlessValidationAlgebra[F, TransactionSemanticError, IoTransaction]] {
            val m = mock[ContextlessValidationAlgebra[F, TransactionSemanticError, IoTransaction]]
            val e: TransactionSemanticError =
              TransactionSemanticErrors.InputDataMismatch(arbitrarySpentTransactionOutput.arbitrary.first)
            (m.validate(_))
              .expects(*)
              .once()
              .returning(e.invalidNec[IoTransaction].pure[F])
            m
          }
          authValidation <- Resource.pure[F, TransactionAuthorizationVerifier[F]] {
            val m = mock[TransactionAuthorizationVerifier[F]]
            (m.validate(_: DynamicContext[F, String, Datum])(_: IoTransaction))
              .expects(*, *)
              .never()
            m
          }
          underTest <- BlockPackerValidation.make[F](dataValidation, authValidation)
          _         <- underTest.transactionIsValid(tx, 0, 0).map(!_).assert.toResource
        } yield ()

      // Test: Valid Data + Invalid Auth
      val test2Resource =
        for {
          dataValidation <- Resource.pure[F, ContextlessValidationAlgebra[F, TransactionSemanticError, IoTransaction]] {
            val m = mock[ContextlessValidationAlgebra[F, TransactionSemanticError, IoTransaction]]
            (m.validate(_))
              .expects(tx)
              .once()
              .returning(tx.validNec[TransactionSemanticError].pure[F])
            m
          }
          authValidation <- Resource.pure[F, TransactionAuthorizationVerifier[F]] {
            val m = mock[TransactionAuthorizationVerifier[F]]
            val e: TransactionAuthorizationError =
              TransactionAuthorizationError.AuthorizationFailed()
            (m.validate(_: DynamicContext[F, String, Datum])(_: IoTransaction))
              .expects(*, tx)
              .once()
              .returning(e.asLeft[IoTransaction].pure[F])
            m
          }
          underTest <- BlockPackerValidation.make[F](dataValidation, authValidation)
          _         <- underTest.transactionIsValid(tx, 0, 0).map(!_).assert.toResource
        } yield ()

      // Test: Valid Data + Valid Auth
      val test3Resource =
        for {
          dataValidation <- Resource.pure[F, ContextlessValidationAlgebra[F, TransactionSemanticError, IoTransaction]] {
            val m = mock[ContextlessValidationAlgebra[F, TransactionSemanticError, IoTransaction]]
            (m.validate(_))
              .expects(tx)
              .once()
              .returning(tx.validNec[TransactionSemanticError].pure[F])
            m
          }
          authValidation <- Resource.pure[F, TransactionAuthorizationVerifier[F]] {
            val m = mock[TransactionAuthorizationVerifier[F]]
            (m.validate(_: DynamicContext[F, String, Datum])(_: IoTransaction))
              .expects(*, tx)
              .once()
              .returning(tx.asRight[TransactionAuthorizationError].pure[F])
            m
          }
          underTest <- BlockPackerValidation.make[F](dataValidation, authValidation)
          _         <- underTest.transactionIsValid(tx, 0, 0).assert.toResource
        } yield ()
      List(test1Resource, test2Resource, test3Resource).sequence.use_
    }
  }

  private def lvlValue(quantity: BigInt): Value =
    Value().withLvl(Value.LVL(Int128(ByteString.copyFrom(quantity.toByteArray))))

  private val emptyLockAddress = LockAddress(id = LockId(ByteString.copyFrom(Array.fill(32)(0.toByte))))

  private def stxo(tx: IoTransaction, index: Int) =
    SpentTransactionOutput(
      TransactionOutputAddress(id = tx.id).withIndex(index),
      Attestation().withPredicate(Attestation.Predicate.defaultInstance),
      tx.outputs(index).value
    )
}
