package co.topl.models

/**
 * Represents _some_ of the data of a Block Header.  The subset of represented data is primarily used for chain
 * selection, eta, and VRF/eligibility verification.  A Block Header includes an operational certificate which
 * unnecessarily consumes memory.
 */
case class SlotDataLegacy(slotId: SlotId, parentSlotId: SlotId, rho: Rho, eta: Eta, height: Long)
