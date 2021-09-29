package co.topl.consensus.algebras

import cats.tagless._
import co.topl.models.Rho
import co.topl.models.utility.Ratio

/**
 * Assists with determining eligibility of a particular staker at some slot.
 */
@autoFunctorK
trait LeaderElectionValidationAlgebra[F[_]] {
  def getThreshold(relativeStake: Ratio, slotDiff: Long): F[Ratio]

  def isSlotLeaderForThreshold(threshold: Ratio)(proofHash: Rho): F[Boolean]
}
