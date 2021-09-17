package co.topl.algebras

import co.topl.models.{PrivateKeys, Slot}

/**
 * A KeyEvolverAlgebra is responsible for encapsulating a key locally and emitting an evolved version of
 * it for some particular slot on demand
 */
trait KeyEvolverAlgebra[F[_]] {
  def evolvedKey(slot: Slot): F[PrivateKeys.Kes]
}
