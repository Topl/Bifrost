package co.topl.consensus.algebras

import co.topl.models.{Box, SlotId, TaktikosAddress}

trait RegistrationLookupAlgebra[F[_]] {

  def registrationOf(slotId: SlotId, address: TaktikosAddress): F[Option[Box.Values.TaktikosRegistration]]

}
