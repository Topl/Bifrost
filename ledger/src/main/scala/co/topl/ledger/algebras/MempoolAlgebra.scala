package co.topl.ledger.algebras

import co.topl.brambl.models.TransactionId
import co.topl.consensus.models.BlockId
import co.topl.ledger.models.MempoolGraph
import fs2.concurrent.Topic

trait MempoolAlgebra[F[_]] {

  /**
   * Read the set of unconfirmed Transaction IDs at the given block ID
   */
  def read(blockId: BlockId): F[MempoolGraph]

  /**
   * Inserts an externally sourced Transaction ID into the Mempool,
   * return true if tx had been added to memory pool, false otherwise
   */
  def add(transactionId: TransactionId): F[Boolean]

  /**
   * Remove/evict the given Transaction ID from the Mempool
   */
  def remove(transactionId: TransactionId): F[Unit]

  /**
   * Check the set of unconfirmed Transaction IDs at the given block ID, if it contains a specific Transaction ID
   */
  def contains(blockId: BlockId, transactionId: TransactionId): F[Boolean]

  /**
   * A topic of transaction IDs that have been included in this mempool
   */
  def adoptions: Topic[F, TransactionId]

}
