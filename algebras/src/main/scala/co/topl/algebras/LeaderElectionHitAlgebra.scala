package co.topl.algebras

import co.topl.models.utility.Ratio
import co.topl.models.{Eta, Slot, Vrf}

/**
 * Assists with constructing VRF Hits at some particular slot.  Interpreters are meant to encapsulate
 * a private VRF Key
 */
trait LeaderElectionHitAlgebra[F[_]] {

  def getHit(
    relativeStake: Ratio,
    slot:          Slot,
    slotDiff:      Long,
    eta:           Eta
  ): F[Option[Vrf.Hit]]
}
