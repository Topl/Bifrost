package co.topl.consensus

import cats.data.OptionT
import co.topl.models.utility.Ratio
import co.topl.models.{Epoch, Nonce, TaktikosAddress}

trait EpochNoncesAlgebra[F[_]] {
  def nonceForEpoch(epoch: Epoch): OptionT[F, Nonce]
}

trait RelativeStateLookupAlgebra[F[_]] {

  /**
   * Retrieves the relative stake corresponding to the provided address within the provided epoch
   */
  def lookup(epoch: Epoch)(address: TaktikosAddress): OptionT[F, Ratio]
}
