package co.topl.minting.algebras

import co.topl.models.{SecretKeys, Slot}

/**
 * A KeyEvolverAlgebra is responsible for encapsulating a key locally and emitting an evolved version of
 * it for some particular slot on demand
 */
trait KeyEvolverAlgebra[F[_]] {
  def evolvedKey(slot: Slot): F[SecretKeys.SymmetricMMM]
}
