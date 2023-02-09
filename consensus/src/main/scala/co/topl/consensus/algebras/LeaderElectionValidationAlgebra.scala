package co.topl.consensus.algebras

import co.topl.models.Rho
import co.topl.models.utility.Ratio

/**
 * Assists with determining eligibility of a particular staker at some slot.
 */
trait LeaderElectionValidationAlgebra[F[_]] {

  /**
   * TODO https://topl.atlassian.net/browse/BN-845
   * @param relativeStake
   * @param slotDiff
   * @return
   */
  def getThreshold(relativeStake: Ratio, slotDiff: Long): F[Ratio]

  /**
   * TODO https://topl.atlassian.net/browse/BN-845
   * @param threshold
   * @param rho
   * @return
   */
  def isSlotLeaderForThreshold(threshold: Ratio)(rho: Rho): F[Boolean]
}
