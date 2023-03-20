package co.topl.genusLibrary.algebras

import co.topl.consensus.models.{BlockHeader, BlockId}
import co.topl.genusLibrary.model.GenusException

/**
 * Vertex finder on the stored Ledger
 * @tparam F the effect-ful context to retrieve the value in
 */
trait VertexFetcherAlgebra[F[_]] {

  /**
   * Fetch header on the stored Ledger
   * @param header actual header to use to find the header vertex on the stored Ledger
   * @return header vertex
   */
  def fetchHeader(blockId: BlockId): F[Either[GenusException, BlockHeader]]

  /**
   * Fetch header given height on the stored Ledger
   *
   * @param height actual header height value
   * @return previous header vertex
   */
  def fetchHeaderByHeight(height: Long): F[Either[GenusException, BlockHeader]]

}
