package co.topl.minting.algebras

import co.topl.models.{Proofs, SecretKeys, Slot}

/**
 * A KeyEvolverAlgebra is responsible for encapsulating a key locally and emitting an evolved version of
 * it for some particular slot on demand
 */
trait KeyEvolverAlgebra[F[_]] {
  def evolveKey(slot: Slot): F[Option[KeyEvolverOut]]
}

case class KeyEvolverOut(slot: Slot, sk: SecretKeys.Ed25519, proofOfVk: Proofs.Knowledge.KesProduct)
