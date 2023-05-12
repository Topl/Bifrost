package co.topl.ledger.algebras

import co.topl.brambl.models.TransactionId
import co.topl.consensus.models.BlockId

trait MempoolAlgebra[F[_]] {

  /**
   * Read the set of unconfirmed Transaction IDs at the given block ID
   */
  def read(blockId: BlockId): F[Set[TransactionId]]

  /**
   * Inserts an externally sourced Transaction ID into the Mempool
   */
  def add(transactionId: TransactionId): F[Unit]

  /**
   * Remove/evict the given Transaction ID from the Mempool
   */
  def remove(transactionId: TransactionId): F[Unit]

  /**
   * Check the set of unconfirmed Transaction IDs at the given block ID, if it contains a specific Transaction ID
   */
  def contains(blockId: BlockId, transactionId: TransactionId): F[Boolean]

}
