package co.topl.genusLibrary.orientDb

import co.topl.consensus.models.{BlockHeader, BlockId}
import co.topl.genusLibrary.failure.Failure

/**
 * Vertex finder on the stored Ledger
 * @tparam F the effect-ful context to retrieve the value in
 */
trait VertexFetcher[F[_]] {

  /**
   * Fetch header on the stored Ledger
   * @param header actual header to use to find the header vertex on the stored Ledger
   * @return header vertex
   */
  def fetchHeader(blockId: BlockId): F[Either[Failure, BlockHeader]]

  /**
   * Fetch header given height on the stored Ledger
   *
   * @param height actual header height value
   * @return previous header vertex
   */
  def fetchHeaderByHeight(height: Long): F[Either[Failure, BlockHeader]]

}
