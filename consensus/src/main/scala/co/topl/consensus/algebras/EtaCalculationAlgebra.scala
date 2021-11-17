package co.topl.consensus.algebras

import co.topl.models.{Eta, Slot, SlotId}

trait EtaCalculationAlgebra[F[_]] {

  /**
   * Determines the eta value at the requested `childSlot` along the tine containing the `parentSlotId`.  In cases
   * where epochOf(childSlot) =!= epochOf(parentSlotId.slot), a new eta value may be calculated.
   */
  def etaToBe(parentSlotId: SlotId, childSlot: Slot): F[Eta]
}
