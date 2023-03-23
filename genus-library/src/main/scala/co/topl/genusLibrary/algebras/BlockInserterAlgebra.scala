package co.topl.genusLibrary.algebras

import co.topl.genusLibrary.model.{BlockData, GenusException}

/**
 * Inserter of blocks to the chain in the data store.
 * @tparam F the effect-ful context to retrieve the value in
 */
trait BlockInserterAlgebra[F[_]] {

  /**
   * Inserts a block to the chain in the data store.
   * @param block the block to be inserted in the data store.
   * @return Unit
   */
  def insert(block: BlockData): F[Either[GenusException, Unit]]

}
