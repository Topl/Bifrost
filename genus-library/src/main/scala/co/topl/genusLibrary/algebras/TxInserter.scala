package co.topl.genusLibrary.algebras

import co.topl.genusLibrary.failure.Failure
import co.topl.genusLibrary.model.BlockData

/**
 * Inserter of block transactions to the chain in the data store.
 * @tparam F the effect-ful context to retrieve the value in
 */
trait TxInserter[F[_]] {

  /**
   * Inserts block transactions to the chain in the data store
   *
   * @param block the full block data for context reasons
   * @return unit
   */
  def insert(
    block: BlockData
  ): F[Either[Failure, Unit]]

}
