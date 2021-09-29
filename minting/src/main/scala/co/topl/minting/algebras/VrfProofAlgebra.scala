package co.topl.minting.algebras

import cats.tagless.autoFunctorK
import co.topl.models.{Epoch, Eta, Proofs, Rho, Slot}

@autoFunctorK
trait VrfProofAlgebra[F[_]] {
  def precomputeForEpoch(epoch: Epoch, previousEta: Eta): F[Unit]
  def testProofForSlot(slot:    Slot, eta:          Eta): F[Proofs.Signature.VrfEd25519]
  def rhoForSlot(slot:          Slot, eta:          Eta): F[Rho]
  def nonceProofForSlot(slot:   Slot, eta:          Eta): F[Proofs.Signature.VrfEd25519]
}
