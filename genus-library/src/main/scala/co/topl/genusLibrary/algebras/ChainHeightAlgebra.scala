package co.topl.genusLibrary.algebras

import co.topl.genusLibrary.failure.Failure

/**
 * Represents the current height of the chain in the data store.
 *
 * @tparam F the effect-ful context to retrieve the value in
 */
trait ChainHeightAlgebra[F[_]] {

  /**
   * Retrieves the current height of the chain in the data store.
   * @return the current height
   */
  def get: F[Either[Failure, Long]]
}
