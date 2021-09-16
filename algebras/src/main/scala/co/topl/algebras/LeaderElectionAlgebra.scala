package co.topl.algebras

import co.topl.models.utility.Ratio
import co.topl.models.{Eta, PrivateKeys, Rho, Slot, Vrf}

trait LeaderElectionAlgebra[F[_]] {

  def nextHit(
    relativeStake: Ratio,
    slot:          Slot,
    maxSlot:       Long,
    eta:           Eta
  ): F[Option[Vrf.Hit]]

  def getHit(
    relativeStake: Ratio,
    slot:          Slot,
    slotDiff:      Long,
    eta:           Eta
  ): F[Option[Vrf.Hit]]

  def getThreshold(relativeStake: Ratio, slotDiff: Long): F[Ratio]

  def isSlotLeaderForThreshold(threshold: Ratio)(proofHash: Rho): F[Boolean]
}
