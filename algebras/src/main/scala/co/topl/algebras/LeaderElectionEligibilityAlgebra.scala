package co.topl.algebras

import co.topl.models.Rho
import co.topl.models.utility.Ratio

/**
 * Assists with determining eligibility of a particular staker at some slot.
 */
trait LeaderElectionEligibilityAlgebra[F[_]] {
  def getThreshold(relativeStake: Ratio, slotDiff: Long): F[Ratio]

  def isSlotLeaderForThreshold(threshold: Ratio)(proofHash: Rho): F[Boolean]
}
