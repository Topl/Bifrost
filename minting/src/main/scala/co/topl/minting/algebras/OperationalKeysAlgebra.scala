package co.topl.minting.algebras

import co.topl.models.{Eta, Proofs, SecretKeys, Slot, SlotId}

/**
 * A KeyEvolverAlgebra is responsible for encapsulating a key locally and emitting an evolved version of
 * it for some particular slot on demand
 */
trait OperationalKeysAlgebra[F[_]] {
  def operationalKeyForSlot(slot: Slot, parentSlotId: SlotId): F[Option[OperationalKeyOut]]
}

case class OperationalKeyOut(slot: Slot, sk: SecretKeys.Ed25519, proofOfVk: Proofs.Knowledge.KesProduct)
