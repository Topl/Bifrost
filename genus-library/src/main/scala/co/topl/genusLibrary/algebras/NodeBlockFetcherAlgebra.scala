package co.topl.genusLibrary.algebras

import co.topl.genus.services.BlockData
import co.topl.genusLibrary.model.GE

/**
 * Algebra which defines fetch operations of blocks against the chain in the Node.
 * @tparam F the effect-ful context to retrieve the value in
 * @tparam G sequence container. Ex: Stream, Seq
 */
trait NodeBlockFetcherAlgebra[F[_], G[_]] {

  /**
   * Fetch sequence of blocks on the chain
   * @param startHeight start from this height
   * @param endHeight end on this height
   * @return effect-ful context of a sequence container of block data
   */
  def fetch(startHeight: Long, endHeight: Long): F[G[BlockData]]

  /**
   * Look-up a block on the chain with a given height
   *
   * @param height The height to lookup
   * @return the full block
   */
  def fetch(height: Long): F[Either[GE, Option[BlockData]]]

  /**
   * Look-up up to the node's current head height
   *
   * @return height
   */
  def fetchHeight(): F[Either[GE, Long]]

}
