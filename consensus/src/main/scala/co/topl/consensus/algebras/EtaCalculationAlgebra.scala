package co.topl.consensus.algebras

import co.topl.models.{Epoch, Eta}

trait EtaCalculationAlgebra[F[_]] {
  def calculate(epoch: Epoch): F[Eta]
}
