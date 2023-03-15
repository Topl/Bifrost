package co.topl.ledger.algebras

import co.topl.brambl.models.Identifier
import co.topl.consensus.models.BlockId

trait MempoolAlgebra[F[_]] {

  /**
   * Read the set of unconfirmed Transaction IDs at the given block ID
   */
  def read(blockId: BlockId): F[Set[Identifier.IoTransaction32]]

  /**
   * Inserts an externally sourced Transaction ID into the Mempool
   */
  def add(transactionId: Identifier.IoTransaction32): F[Unit]

  /**
   * Remove/evict the given Transaction ID from the Mempool
   */
  def remove(transactionId: Identifier.IoTransaction32): F[Unit]

}
