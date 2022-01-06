package co.topl.minting.algebras

import co.topl.models._
import co.topl.models.utility.Ratio

import scala.collection.immutable.NumericRange

trait VrfProofAlgebra[F[_]] {
  def precomputeForEpoch(epoch: Epoch, previousEta: Eta): F[Unit]

  def rhoForSlot(slot: Slot, eta: Eta): F[Rho]

  def proofForSlot(slot: Slot, eta: Eta): F[Proofs.Knowledge.VrfEd25519]

  /**
   * Determine which slots will be guaranteed to be ineligible in the provided epoch
   * @param inRange An optional range of slots to bound the result within the epoch
   */
  def ineligibleSlots(
    epoch:         Epoch,
    eta:           Eta,
    inRange:       Option[NumericRange.Exclusive[Long]],
    relativeStake: Ratio
  ): F[Vector[Slot]]
}
