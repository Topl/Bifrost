package co.topl.algebras

import co.topl.models.utility.Ratio
import co.topl.models.{BlockHeaderV2, Slot, TaktikosAddress}

trait VrfRelativeStakeLookupAlgebra[F[_]] {

  /**
   * Retrieves the relative stake corresponding to the provided address in the N-2 epoch of the given block
   * @param block A block belonging to some Tine
   * @param currentSlot For validation purposes, this value is usually just block.slot.  When minting a new block, this
   *                    value corresponds to the forward-moving slot when determining VRF eligibility.
   */
  def lookupAt(block: BlockHeaderV2, currentSlot: Slot)(address: TaktikosAddress): F[Option[Ratio]]
}
