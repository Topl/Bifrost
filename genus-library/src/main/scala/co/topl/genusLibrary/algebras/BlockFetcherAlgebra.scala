package co.topl.genusLibrary.algebras

import co.topl.models.{BlockHeaderV2, BlockV2}

/**
 * Fetcher of blocks on the chain.
 * @tparam F the effect-ful context to retrieve the value in
 * @tparam G sequence container. Ex: Stream, Seq
 */
trait BlockFetcherAlgebra[F[_], G[_]] {

  /**
   * Look-up block headers on the chain, starting from a given inclusive height.
   * @param height The height to start the lookup. It's inclusive.
   * @return a sequence container of headers
   */
  def fetchHeaders(height: Long): SequenceResponse[F, G, Option[BlockHeaderV2]]

  /**
   * Look-up block bodies on the chain, starting from a given inclusive height.
   *
   * @param height The height to start the lookup. It's inclusive.
   * @return a sequence container of headers
   */
  // def fetchBodies(height: Long): SequenceResponse[F, G, Option[BlockBodyV2]]

  /**
   * Look-up block transactions on the chain, starting from a given inclusive height.
   *
   * @param height The height to start the lookup. It's inclusive.
   * @return a sequence container of headers
   */
  // def fetchTransactions(height: Long): SequenceResponse[Option[BlockBodyV2.Full]]

  /**
   * Look-up a block on the chain with a given height
   *
   * @param height The height to lookup
   * @return the full block
   */
  def fetch(height: Long): ServiceResponse[F, Option[BlockV2.Full]]

}
