package co.topl.minting.interpreters

import munit.CatsEffectSuite
import munit.ScalaCheckEffectSuite
import cats.implicits._
import cats.effect.implicits._
import org.scalamock.munit.AsyncMockFactory
import cats.effect.IO
import co.topl.ledger.algebras.MempoolAlgebra
import co.topl.consensus.models.BlockId
import co.topl.ledger.models.MempoolGraph
import co.topl.ledger.algebras.BoxStateAlgebra
import co.topl.ledger.algebras.TransactionRewardCalculatorAlgebra
import co.topl.brambl.validation.algebras._
import co.topl.brambl.syntax._
import co.topl.models.generators.consensus.ModelGenerators
import co.topl.models.ModelGenerators._
import co.topl.node.models.FullBlockBody

import scala.concurrent.duration._
import java.util.concurrent.TimeoutException
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.models.Datum
import co.topl.brambl.models.transaction.UnspentTransactionOutput
import co.topl.brambl.models.box.Value
import com.google.protobuf.ByteString
import quivr.models.Int128
import co.topl.brambl.models.transaction.SpentTransactionOutput
import co.topl.brambl.models.TransactionOutputAddress
import co.topl.quivr.runtime.DynamicContext
import co.topl.brambl.models.LockAddress
import co.topl.brambl.models.LockId
import co.topl.brambl.models.box.Attestation

class GraphBlockPackerSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {
  type F[A] = IO[A]

  test("return empty for empty mempool") {
    withMock {
      val mempool = mock[MempoolAlgebra[F]]
      (mempool.read(_: BlockId)).expects(*).once().returning(MempoolGraph.empty.pure[F])
      val testResource =
        for {
          underTest <- GraphBlockPacker.make[F](
            mempool,
            mock[BoxStateAlgebra[F]],
            mock[TransactionRewardCalculatorAlgebra[F]],
            mock[TransactionCostCalculator[F]],
            mock[TransactionAuthorizationVerifier[F]]
          )
          iterative <- underTest.improvePackedBlock(ModelGenerators.arbitraryBlockId.arbitrary.first, 0, 0).toResource
          _ <- iterative.improve(FullBlockBody.defaultInstance).timeout(1.second).intercept[TimeoutException].toResource
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
      val mempoolGraph = MempoolGraph.empty.add(tx2).add(tx3).add(tx4)
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
      val rewardCalculator = mock[TransactionRewardCalculatorAlgebra[F]]
      (rewardCalculator.rewardOf(_: IoTransaction)).expects(*).anyNumberOfTimes().returning(BigInt(100).pure[F])
      val costCalculator = mock[TransactionCostCalculator[F]]
      (costCalculator.costOf(_: IoTransaction)).expects(*).anyNumberOfTimes().returning(50L.pure[F])
      val authorizationVerifier = mock[TransactionAuthorizationVerifier[F]]
      (authorizationVerifier
        .validate(_: DynamicContext[F, String, Datum])(_: IoTransaction))
        .expects(*, *)
        .anyNumberOfTimes()
        .onCall { case (_: DynamicContext[F, String, Datum], tx: IoTransaction) =>
          tx.asRight.pure[F]
        }
      val testResource =
        for {
          underTest <- GraphBlockPacker.make[F](
            mempool,
            boxState,
            rewardCalculator,
            costCalculator,
            authorizationVerifier
          )
          iterative <- underTest.improvePackedBlock(ModelGenerators.arbitraryBlockId.arbitrary.first, 0, 0).toResource
          result <- fs2.Stream
            .unfoldEval(FullBlockBody.defaultInstance)(iterative.improve(_).map(v => (v, v).some))
            .interruptAfter(3.seconds)
            .compile
            .lastOrError
            .toResource
          _ <- IO(result.transactions).assertEquals(List(tx2, tx3, tx4)).toResource
        } yield ()

      testResource.use_
    }
  }

  private def lvlValue(quantity: BigInt) =
    Value().withLvl(Value.LVL(Int128(ByteString.copyFrom(quantity.toByteArray))))

  private val emptyLockAddress = LockAddress(id = LockId(ByteString.copyFrom(Array.fill(32)(0.toByte))))

  private def stxo(tx: IoTransaction, index: Int) =
    SpentTransactionOutput(
      TransactionOutputAddress(id = tx.id).withIndex(index),
      Attestation().withPredicate(Attestation.Predicate.defaultInstance),
      tx.outputs(index).value
    )
}
