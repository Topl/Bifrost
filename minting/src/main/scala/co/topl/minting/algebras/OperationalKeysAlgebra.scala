package co.topl.minting.algebras

import co.topl.models.{Proofs, SecretKeys, Slot, SlotId, VerificationKeys}

/**
 * A KeyEvolverAlgebra is responsible for encapsulating a key locally and emitting an evolved version of
 * it for some particular slot on demand
 */
trait OperationalKeysAlgebra[F[_]] {
  def operationalKeyForSlot(slot: Slot, parentSlotId: SlotId): F[Option[OperationalKeyOut]]
}

case class OperationalKeyOut(
  slot:            Slot,
  childSK:         SecretKeys.Ed25519,
  parentSignature: Proofs.Knowledge.KesProduct,
  parentVK:        VerificationKeys.KesProduct
)
