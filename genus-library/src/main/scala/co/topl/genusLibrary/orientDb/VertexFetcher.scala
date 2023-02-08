package co.topl.genusLibrary.orientDb

import co.topl.consensus.models.BlockHeader
import co.topl.genusLibrary.failure.Failure
import co.topl.genusLibrary.orientDb.wrapper.WrappedVertex
import co.topl.node.models.BlockBody

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
  def fetchHeader(header: BlockHeader): F[Either[Failure, WrappedVertex]]

  /**
   * Fetch previous header on the stored Ledger
   *
   * @param header actual header to use to find the previous header vertex on the stored Ledger
   * @return previous header vertex
   */
  def fetchPreviousHeader(header: BlockHeader): F[Either[Failure, WrappedVertex]]

  /**
   * Fetch body on the stored Ledger
   *
   * @param body actual body to use to find the body vertex on the stored Ledger
   * @param blockHeight block height that corresponds to the given block body
   * @return body vertex
   */
  def fetchBody(body: BlockBody, blockHeight: Long): F[Either[Failure, WrappedVertex]]

}
