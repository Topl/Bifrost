package co.topl.minting.algebras

import co.topl.models.{Slot, TaktikosAddress}
import co.topl.models.utility.Ratio

trait VrfRelativeStakeMintingLookupAlgebra[F[_]] {

  /**
   * Retrieves the relative stake corresponding to the provided address in the N-2 epoch of the given block
   * @param block A block belonging to some Tine
   * @param globalSlot For validation purposes, this value is usually just block.slot.  When minting a new block, this
   *                    value corresponds to the forward-moving slot when determining VRF eligibility.
   */
  def lookupAt(globalSlot: Slot, address: TaktikosAddress): F[Option[Ratio]]
}
