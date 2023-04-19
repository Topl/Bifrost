package co.topl.genusLibrary.algebras

import co.topl.consensus.models.{BlockHeader, BlockId}
import co.topl.genus.services.BlockData
import co.topl.genusLibrary.model.GE
import co.topl.node.models.BlockBody

/**
 * Algebra which defines fetch operations of blocks against the stored Ledger.
 *
 * @tparam F the effect-ful context to retrieve the value in
 */
trait BlockFetcherAlgebra[F[_]] {

  /**
   * Fetch Canonical head vertex on the stored Ledger
   *
   * @return Optional header vertex, None if it was not found
   */
  def fetchCanonicalHead(): F[Either[GE, Option[BlockHeader]]]

  /**
   * Fetch a BlockHeader on the stored Ledger
   * @param blockId  blockId filter by field
   * @return Optional BlockHeader, None if it was not found
   */
  def fetchHeader(blockId: BlockId): F[Either[GE, Option[BlockHeader]]]

  /**
   * Fetch a BlockBody on the stored Ledger
   *
   * @param blockId blockId filter by field
   * @return Optional BlockBody, None if it was not found
   */
  def fetchBody(blockId: BlockId): F[Either[GE, Option[BlockBody]]]

  /**
   * Fetch a Block on the stored Ledger given a blockId
   *
   * @param blockId fetch a BlockData on the stored Ledger given a blockId
   * @return Optional BlockData, None if it was not found
   */
  def fetchBlock(blockId: BlockId): F[Either[GE, Option[BlockData]]]

  /**
   * Fetch a BlockHeader with height filter on the stored Ledger
   *
   * @param height height filter by field
   * @return Optional BlockHeader, None if it was not found
   */
  def fetchHeaderByHeight(height: Long): F[Either[GE, Option[BlockHeader]]]

  /**
   * Fetch a BlockData with height filter on the stored Ledger
   *
   * @param height filter by field
   * @return Optional BlockData, None if it was not found
   */
  def fetchBlockByHeight(height: Long): F[Either[GE, Option[BlockData]]]

  def fetchBlockByDepth(height: Long): F[Either[GE, Option[BlockData]]]

}
