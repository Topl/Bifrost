package co.topl.minting.algebras

import cats.tagless.autoFunctorK
import co.topl.models.utility.Ratio
import co.topl.models.{EligibilityCertificate, Eta, Slot}

/**
 * Assists with constructing VRF Hits at some particular slot.  Interpreters are meant to encapsulate
 * a private VRF Key
 */
@autoFunctorK
trait LeaderElectionMintingAlgebra[F[_]] {

  def getHit(
    relativeStake: Ratio,
    slot:          Slot,
    slotDiff:      Long,
    eta:           Eta
  ): F[Option[LeaderElectionMintingAlgebra.VrfHit]]
}

object LeaderElectionMintingAlgebra {
  case class VrfHit(cert: EligibilityCertificate, slot: Slot, threshold: Ratio)
}
