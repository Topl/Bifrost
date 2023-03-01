package co.topl.genusLibrary.algebras

import co.topl.genusLibrary.failure.Failure
import co.topl.genusLibrary.model.BlockData
import co.topl.brambl.models.Address

/**
 * Inserter of addresses to the chain in the data store.
 * @tparam F the effect-ful context to retrieve the value in
 */
trait AddressInserter[F[_]] {

  /**
   * Inserts address to the chain in the data store
   *
   * @param address the address to be inserted in the data store
   * @param block   the full block data for context reasons
   * @return unit
   */
  def insert(
    address: Address,
    block:   BlockData
  ): F[Either[Failure, Unit]]

}
