package co.topl.minting.algebras

import co.topl.minting.models.OperationalKeyOut
import co.topl.models.Slot
import co.topl.consensus.models.SlotId

/**
 * A KeyEvolverAlgebra is responsible for encapsulating a key locally and emitting an evolved version of
 * it for some particular slot on demand
 */
trait OperationalKeyMakerAlgebra[F[_]] {
  def operationalKeyForSlot(slot: Slot, parentSlotId: SlotId): F[Option[OperationalKeyOut]]
}
