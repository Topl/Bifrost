package co.topl.ledger.interpreters

import cats.effect._
import cats.effect.implicits._
import cats.implicits._
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.box.Value
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.syntax.ioTransactionAsTransactionSyntaxOps
import co.topl.brambl.validation.algebras.TransactionCostCalculator
import co.topl.ledger.algebras.BlockBodyScoreAlgebra
import co.topl.ledger.algebras.TransactionRewardCalculatorAlgebra
import com.github.benmanes.caffeine.cache.Caffeine
import scalacache.Entry
import scalacache.caffeine.CaffeineCache

object BlockBodyScore {

  def make[F[_]: Sync](
    transactionCostCalculator:          TransactionCostCalculator[F],
    transactionRewardCalculatorAlgebra: TransactionRewardCalculatorAlgebra[F],
    cacheSize:                          Int = 512
  ): Resource[F, BlockBodyScoreAlgebra[F]] =
    for {
      cache <- Sync[F]
        .delay(
          CaffeineCache[F, TransactionId, BigInt](
            Caffeine.newBuilder.maximumSize(cacheSize).build[TransactionId, Entry[BigInt]]
          )
        )
        .toResource
    } yield new BlockBodyScoreAlgebra[F] {

      /**
       * Calculates the "cost" of the Transaction, using the `transactionCostCalculator`
       * @param transaction The transaction to consider
       * @return a BigInt representing the aggregate "cost" of the transaction
       */
      private def transactionCost(transaction: IoTransaction): F[BigInt] =
        Sync[F]
          .defer(transactionCostCalculator.costOf(transaction))
          .map(BigInt(_))

      /**
       * Calculates the "benefit" of the Transaction.  The "benefit" is simply a normalized numeric value to
       * represent the aggregate "rewards" of the Transaction.  For example, LVLs may be multiplied by some factor,
       * while TOPLs may be multipled by another factor, while Assets may be completely ignored.
       * @param transaction The transaction to consider
       * @return a BigInt representing the aggregate "benefit" of the transaction
       */
      private def transactionBenefit(transaction: IoTransaction): F[BigInt] =
        Sync[F]
          .defer(transactionRewardCalculatorAlgebra.rewardsOf(transaction))
          .map(_.map(_.value).foldMap {
            // The reward calculator could return multiple value types, not just LVLs.
            // We handle just the LVL and TOPL cases here.
            case v: Value.Value.Lvl  => BigInt(v.value.quantity.value.toByteArray)
            case v: Value.Value.Topl => BigInt(v.value.quantity.value.toByteArray)
            case _                   => BigInt(0)
          })

      /**
       * Calculate-and-cache the score of the Transaction by deducting the cost from the benefit
       * @param transaction The transaction to consider
       * @return a BigInt representing the net score of the Transaction
       */
      private def transactionScore(transaction: IoTransaction): F[BigInt] =
        cache.cachingF(transaction.id)(ttl = None)(
          Sync[F].defer(
            (transactionBenefit(transaction), transactionCost(transaction)).mapN(_ - _)
          )
        )

      def scoreOf(candidate: Seq[IoTransaction]): F[BigInt] = candidate.foldMapM(transactionScore)
    }

}
