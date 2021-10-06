package co.topl.minting.algebras

import co.topl.models.{Eta, Slot}

trait EtaMintingAlgebra[F[_]] {

  /**
   * Not referentially transparent
   * "What is the eta value for the certificate included in the newly minted block?"
   * @param globalSlot
   * @return The N-1 epoch nonce
   */
  def etaOf(globalSlot: Slot): F[Eta]
}
