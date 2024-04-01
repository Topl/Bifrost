package co.topl.ledger.algebras

import co.topl.brambl.models.transaction.IoTransaction
import co.topl.ledger.models.RewardQuantities

trait TransactionRewardCalculatorAlgebra {

  /**
   * Calculates the provided rewards of the given transaction.  Any "excess" value is treated as a Reward
   * @param transaction The transaction containing the fee/reward
   * @return a BigInt representing the LVL fee/reward
   */
  def rewardsOf(transaction: IoTransaction): RewardQuantities

}
