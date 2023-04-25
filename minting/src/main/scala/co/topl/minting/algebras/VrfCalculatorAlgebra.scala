package co.topl.minting.algebras

import co.topl.models._

trait VrfCalculatorAlgebra[F[_]] {

  def rhoForSlot(slot: Slot, eta: Eta): F[Rho]

  def proofForSlot(slot: Slot, eta: Eta): F[Bytes]

}
