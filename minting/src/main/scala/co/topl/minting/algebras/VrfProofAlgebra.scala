package co.topl.minting.algebras

import co.topl.models._

trait VrfProofAlgebra[F[_]] {
  def precomputeForEpoch(epoch: Epoch, previousEta: Eta): F[Unit]
  def ineligibleSlots(epoch:    Epoch): F[Vector[Slot]]
  def testProofForSlot(slot:    Slot, eta:          Eta): F[Proofs.Knowledge.VrfEd25519]
  def rhoForSlot(slot:          Slot, eta:          Eta): F[Rho]
  def nonceProofForSlot(slot:   Slot, eta:          Eta): F[Proofs.Knowledge.VrfEd25519]
}
