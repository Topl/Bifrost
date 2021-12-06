package co.topl.minting.algebras

import co.topl.models.{Epoch, Eta, Proofs, Rho, Slot}

trait VrfProofAlgebra[F[_]] {
  def precomputeForEpoch(epoch: Epoch, previousEta: Eta): F[Unit]
  def testProofForSlot(slot:    Slot, eta:          Eta): F[Proofs.Knowledge.VrfEd25519]
  def rhoForSlot(slot:          Slot, eta:          Eta): F[Rho]
  def nonceProofForSlot(slot:   Slot, eta:          Eta): F[Proofs.Knowledge.VrfEd25519]
}
