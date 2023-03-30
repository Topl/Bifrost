package co.topl.consensus.algebras

import co.topl.models.Bytes
import co.topl.models.Slot

/**
 * A cache of recently used VRF eligibilities
 */
trait EligibilityCacheAlgebra[F[_]] {

  /**
   * Attempt to include the given eligibility in the cache
   * @param vrfVK The staker's VRF VK
   * @param slot The slot for which the eligibility was generated
   * @return true if the eligibility was inserted, false if it was already in the cache
   */
  def tryInclude(vrfVK: Bytes, slot: Slot): F[Boolean]

}
