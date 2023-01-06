package co.topl.genusLibrary.algebras

import co.topl.genusLibrary.failure.Failure
import co.topl.genusLibrary.model.BlockData
import co.topl.models.BlockBody

/**
 * Inserter of block transactions to the chain in the data store.
 * @tparam F the effect-ful context to retrieve the value in
 */
trait TxInserter[F[_]] {

  /**
   * Inserts block transactions to the chain in the data store
   *
   * @param transactions the block transactions to be inserted in the data store
   * @param block        the full block data for context reasons
   * @return unit
   */
  def insert(
    transactions: BlockBody.Full,
    block:        BlockData
  ): F[Either[Failure, Unit]]

}
