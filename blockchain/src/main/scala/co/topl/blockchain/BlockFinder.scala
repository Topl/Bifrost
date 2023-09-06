package co.topl.blockchain

import cats.effect.kernel.Async
import cats.implicits._
import co.topl.brambl.models.TransactionId
import co.topl.consensus.models.BlockId
import co.topl.node.models.BlockBody
import fs2._

object BlockFinder {

  /**
   * Searches the given streams of blocks (IDs) for one which contains the requested Transaction ID
   * @param fetchBlockBody a lookup function to retrieve a block body
   * @param blockHistory a backwards-traversal of block IDs (from the current canonical head)
   * @param newBlocks a forwards-traversal of new block adoptions
   * @param id The Transaction ID to find
   * @return The ID of the block containing the transaction
   */
  def forTransactionId[F[_]: Async](
    fetchBlockBody: BlockId => F[BlockBody]
  )(blockHistory: Stream[F, BlockId], newBlocks: Stream[F, BlockId])(id: TransactionId): F[BlockId] =
    blockHistory
      .merge(newBlocks)
      .evalFilter(
        fetchBlockBody(_).map(body => body.transactionIds.contains(id) || body.rewardTransactionId.contains(id))
      )
      .head
      .compile
      .lastOrError

}
