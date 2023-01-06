package co.topl.genusLibrary.algebras

import co.topl.genusLibrary.failure.Failure
import co.topl.genusLibrary.model.BlockData
import co.topl.models.BlockBody

/**
 * Inserter of block bodies to the chain in the data store.
 * @tparam F the effect-ful context to retrieve the value in
 */
trait BodyInserter[F[_]] {

  /**
   * Inserts block body to the chain in the data store
   *
   * @param body the block body to be inserted in the data store
   * @param block the full block data for context reasons
   * @return unit
   */
  def insert(
    body:  BlockBody,
    block: BlockData
  ): F[Either[Failure, Unit]]

}
