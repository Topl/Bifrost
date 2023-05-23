package co.topl.ledger.algebras

import co.topl.brambl.models.box.Value
import co.topl.brambl.models.transaction.IoTransaction

trait TransactionRewardCalculatorAlgebra[F[_]] {

  /**
   * Calculates the fee/reward of the given IoTransaction
   * @param transaction The transaction containing the fee/reward
   * @return A *Set* of different *Values* that can be used as a reward
   */
  def rewardsOf(transaction: IoTransaction): F[Seq[Value]]

}
