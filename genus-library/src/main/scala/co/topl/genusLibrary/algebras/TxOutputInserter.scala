package co.topl.genusLibrary.algebras

import co.topl.genus.services.Txo
import co.topl.genusLibrary.failure.Failure
import co.topl.genusLibrary.model.BlockData

/**
 * Inserter of transaction outputs to the chain in the data store.
 * @tparam F the effect-ful context to retrieve the value in
 */
trait TxOutputInserter[F[_]] {

  /**
   * Inserts transaction output to the chain in the data store
   *
   * @param txo the transaction output to be inserted in the data store
   * @param block the full block data for context reasons
   * @return unit
   */
  def insert(
    txo:   Txo,
    block: BlockData
  ): F[Either[Failure, Unit]]

}
