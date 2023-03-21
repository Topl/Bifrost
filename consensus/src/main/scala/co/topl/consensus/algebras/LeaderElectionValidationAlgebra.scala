package co.topl.consensus.algebras

import co.topl.models.Rho
import co.topl.models.utility.Ratio

/**
 * Assists with determining eligibility of a particular staker at some slot.
 */
trait LeaderElectionValidationAlgebra[F[_]] {

  /**
   * Determines the staker's staking threshold, which is a factor of their relative stake applied to a difficulty curve
   * @param relativeStake The percentage/ratio of the staker's stake, compared to the total active stake of the network
   * @param slotDiff The number of slots since the _parent_ block was minted
   * @return a threshold ratio
   */
  def getThreshold(relativeStake: Ratio, slotDiff: Long): F[Ratio]

  /**
   * Determines if the given staking threshold value, combined with the staker's randomness/rho value, is sufficient to
   * produce a block
   * @param threshold The staker's threshold, as determined by [[getThreshold]]
   * @param rho The staker's rho value
   * @return true if the staker is eligible to produce a block and false otherwise
   */
  def isSlotLeaderForThreshold(threshold: Ratio)(rho: Rho): F[Boolean]
}
