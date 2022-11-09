package co.topl.genusLibrary.algebras

/**
 * Represents the current height of the largest chain in the data store.
 * @tparam F the effect-ful context to retrieve the value in
 */
trait ChainHeightAlgebra[F[_]] {

  /**
   * Retrieves the current height of the largest chain.
   * @return the current height
   */
  def get: StoreResponse[F, Long]
}
