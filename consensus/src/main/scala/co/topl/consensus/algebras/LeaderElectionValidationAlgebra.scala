package co.topl.consensus.algebras

import co.topl.models.Rho
import co.topl.models.utility.Ratio

/**
 * Assists with determining eligibility of a particular staker at some slot.
 */
trait LeaderElectionValidationAlgebra[F[_]] {
  def getThreshold(relativeStake: Ratio, slotDiff: Long): F[Ratio]

  def isSlotLeaderForThreshold(threshold: Ratio)(rho: Rho): F[Boolean]
}
