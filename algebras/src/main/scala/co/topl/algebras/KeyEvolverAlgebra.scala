package co.topl.algebras

import co.topl.models.{PrivateKeys, Slot}

trait KeyEvolverAlgebra[F[_]] {
  def evolvedKey(slot: Slot): F[PrivateKeys.Kes]
}
