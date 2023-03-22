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
   * @param blockId header id used  to find the header vertex on the stored Ledger
   * @return Optional BlockHeader, None if it was not found
   */
  def fetchHeader(blockId: BlockId): F[Either[GenusException, Option[BlockHeader]]]

  /**
   * Fetch header given height on the stored Ledger
   *
   * @param height actual header height value
   * @return Optional BlockHeader, None if it was not found
   */
  def fetchHeaderByHeight(height: Long): F[Either[GenusException, Option[BlockHeader]]]

}
