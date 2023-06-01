package co.topl.ledger.algebras

import co.topl.brambl.models.transaction.IoTransaction

trait TransactionRewardCalculatorAlgebra[F[_]] {

  /**
   * Calculates the LVL fee/reward of the given IoTransaction
   * @param transaction The transaction containing the fee/reward
   * @return a BigInt representing the LVL fee/reward
   */
  def rewardOf(transaction: IoTransaction): F[BigInt]

}
