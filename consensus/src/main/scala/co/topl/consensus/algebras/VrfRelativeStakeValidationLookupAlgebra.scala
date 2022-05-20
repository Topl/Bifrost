package co.topl.consensus.algebras

import co.topl.models.utility.Ratio
import co.topl.models.{SlotId, StakingAddress}

trait VrfRelativeStakeValidationLookupAlgebra[F[_]] {

  /**
   * Retrieves the relative stake corresponding to the provided address in the N-2 epoch of the given block
   * @param block A block belonging to some Tine
   * @param currentSlot For validation purposes, this value is usually just block.slot.  When minting a new block, this
   *                    value corresponds to the forward-moving slot when determining VRF eligibility.
   */
  def lookupAt(slotId: SlotId, address: StakingAddress): F[Option[Ratio]]
}
