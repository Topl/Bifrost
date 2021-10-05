package co.topl.consensus.algebras

import co.topl.models.{Box, SlotId, TaktikosAddress}

trait RegistrationLookupAlgebra[F[_]] {

  /**
   * Lookup the registration of the given TaktikosAddress using the slotId of the consensus state for the epoch
   * @param slotId slotId of the consensus state for the epoch
   * @param address The address to lookup
   * @return An optional registration, if the address was registered
   */
  def registrationOf(slotId: SlotId, address: TaktikosAddress): F[Option[Box.Values.TaktikosRegistration]]

}
