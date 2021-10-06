package co.topl.consensus.algebras

import co.topl.models.{Eta, SlotId}

trait EtaValidationAlgebra[F[_]] {

  /**
   * Referentially transparent
   *
   * Retrieves the eta value of given slotId
   * @return The Eta value corresponding to the provided slotId,
   *         but it is derived from the blocks in the first 2/3 of epoch N-1 of the provided slot
   */
  def etaOf(slotId: SlotId): F[Eta]
}
