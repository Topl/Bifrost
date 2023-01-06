package co.topl.genusLibrary.algebras

import co.topl.genusLibrary.failure.Failure
import co.topl.genusLibrary.model.BlockData
import co.topl.models.BlockHeader

/**
 * Inserter of block headers to the chain in the data store.
 * @tparam F the effect-ful context to retrieve the value in
 */
trait HeaderInserter[F[_]] {

  /**
   * Inserts block header to the chain in the data store
   *
   * @param header the block header to be inserted in the data store
   * @param block  the full block data for context reasons
   * @return unit
   */
  def insert(
    header: BlockHeader,
    block:  BlockData
  ): F[Either[Failure, Unit]]

}
