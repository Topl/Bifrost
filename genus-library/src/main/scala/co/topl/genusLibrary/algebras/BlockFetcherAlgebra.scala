package co.topl.genusLibrary.algebras

import co.topl.genusLibrary.model.BlockData

/**
 * Fetcher of blocks on the chain.
 * @tparam F the effect-ful context to retrieve the value in
 */
trait BlockFetcherAlgebra[F[_], G[_]] {

  /**
   * Look-up a block on the chain with a given height
   * @param height The height to lookup
   * @return the full block
   */
  def fetch(height: Long): SequenceResponse[F, G, Option[BlockData]]

}
