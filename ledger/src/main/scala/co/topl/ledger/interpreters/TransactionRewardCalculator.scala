package co.topl.ledger.interpreters

import cats.effect._
import co.topl.brambl.models.box.Value
import co.topl.ledger.algebras.TransactionRewardCalculatorAlgebra
import com.google.protobuf.ByteString
import quivr.models.Int128

/**
 * Implements a TransactionRewardCalculator which returns unclaimed LVLs as a Reward.  If there are no unclaimed LVLs,
 * no Rewards are returned.
 *
 * TOPLs, Assets, and Registrations may not be rewards.
 */
object TransactionRewardCalculator {

  def make[F[_]: Sync]: Resource[F, TransactionRewardCalculatorAlgebra[F]] =
    Resource.pure(tx =>
      Sync[F].delay {
        val lvlRewardQuantity =
          sumLvls(tx.inputs)(_.value) - sumLvls(tx.outputs)(_.value)
        if (lvlRewardQuantity > 0)
          List(Value(Value.Value.Lvl(Value.LVL(Int128(ByteString.copyFrom(lvlRewardQuantity.toByteArray))))))
        else
          Nil
      }
    )

  /**
   * Extracts LVL Box Values from the given collection, and sums the quantities
   * @param containsValues a collection that contains some value T
   * @param extractValue a function to extract a Value from T
   * @tparam T an abstract type (SpentTransactionOutput or UnspentTransactionOutput)
   * @return a BigInt sum
   */
  private def sumLvls[T](containsValues: Iterable[T])(extractValue: T => Value): BigInt =
    containsValues
      .map(extractValue)
      .flatMap(_.value.lvl)
      .map(_.quantity.value.toByteArray)
      .map(BigInt(_))
      .sum

}
