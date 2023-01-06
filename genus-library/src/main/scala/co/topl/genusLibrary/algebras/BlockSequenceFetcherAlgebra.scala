package co.topl.genusLibrary.algebras

import co.topl.genusLibrary.model.BlockData

/**
 * Fetcher of blocks on the chain.
 * @tparam F the effect-ful context to retrieve the value in
 * @tparam G sequence container. Ex: Stream, Seq
 */
trait BlockSequenceFetcherAlgebra[F[_], G[_]] {

  /**
   * Fetch sequence of blocks on the chain
   * @param startHeight start from this height
   * @param endHeight end on this height
   * @return effect-ful context of a sequence container of block data
   */
  def fetch(startHeight: Long, endHeight: Long): F[G[BlockData]]

}
