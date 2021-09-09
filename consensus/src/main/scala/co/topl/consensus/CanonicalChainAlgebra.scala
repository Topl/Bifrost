package co.topl.consensus

import cats.data.OptionT
import co.topl.models.utility.Ratio
import co.topl.models.{BlockHeaderV2, Eta, TaktikosAddress}

trait EtaAlgebra[F[_]] {

  /**
   * Retrieves the eta value of given block
   * @param block The block being verified
   * @return The N-1 epoch nonce
   */
  def etaOf(block: BlockHeaderV2): F[Eta]
}

trait VrfRelativeStateLookupAlgebra[F[_]] {

  /**
   * Retrieves the relative stake corresponding to the provided address in the N-2 epoch of the given block
   */
  def lookupAt(block: BlockHeaderV2)(address: TaktikosAddress): OptionT[F, Ratio]
}
