package co.topl.algebras

import co.topl.models.{BlockHeaderV2, Eta, Slot}

trait EtaLookupAlgebra[F[_]] {

  /**
   * Retrieves the eta value of given block
   * @param block A block belonging to some Tine
   * @param currentSlot For validation purposes, this value is usually just block.slot.  When minting a new block, this
   *                    value corresponds to the forward-moving slot when determining VRF eligibility.
   * @return The N-1 epoch nonce
   */
  def etaOf(block: BlockHeaderV2, currentSlot: Slot): F[Eta]
}
