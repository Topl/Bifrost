package co.topl.ledger.interpreters

import cats.effect.IO
import cats.effect.implicits._
import cats.implicits._
import munit.CatsEffectSuite
import munit.ScalaCheckEffectSuite
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory
import co.topl.brambl.generators.ModelGenerators._
import co.topl.brambl.models.box.Value
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.syntax.ioTransactionAsTransactionSyntaxOps
import co.topl.brambl.validation.algebras.TransactionCostCalculator
import co.topl.ledger.algebras.TransactionRewardCalculatorAlgebra
import com.google.protobuf.ByteString
import org.scalacheck.Gen
import quivr.models.Int128

class BlockBodyScoreSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  test("Return score of 0 for empty block") {
    val testResource =
      for {
        underTest <- BlockBodyScore.make[F](null, null)
        _         <- underTest.scoreOf(Nil).assertEquals(BigInt(0)).toResource
      } yield ()

    testResource.use_
  }

  test("Return score for a multi-transaction block") {
    val singleGen =
      for {
        tx     <- arbitraryIoTransaction.arbitrary.map(_.embedId)
        cost   <- Gen.posNum[Long]
        reward <- Gen.posNum[Long]
      } yield (tx, cost, reward)
    val multiGen =
      Gen.listOf(singleGen)
    PropF.forAllF(multiGen) { candidate =>
      val costCalculator = new TransactionCostCalculator[F] {
        private val data = candidate.map { case (transaction, cost, _) => transaction -> cost }.toMap
        def costOf(transaction: IoTransaction): F[Long] = data(transaction).pure[F]
      }
      val rewardCalculator = new TransactionRewardCalculatorAlgebra[F] {
        private val data: Map[IoTransaction, List[Value]] =
          candidate.map { case (transaction, _, reward) =>
            transaction -> List(
              Value(
                Value.Value.Lvl(Value.LVL(Int128(ByteString.copyFrom(BigInt(reward).toByteArray))))
              )
            )
          }.toMap

        def rewardsOf(transaction: IoTransaction): F[Seq[Value]] = data(transaction).pure[F]
      }

      val testResource =
        for {
          underTest <- BlockBodyScore.make[F](costCalculator, rewardCalculator)
          expectedCost = candidate.map(_._2).map(BigInt(_)).sum
          expectedReward = candidate.map(_._3).sum
          expectedScore = expectedReward - expectedCost
          _ <- underTest
            .scoreOf(candidate.map(_._1))
            .assertEquals(expectedScore)
            .toResource
        } yield ()

      testResource.use_
    }

  }
}
