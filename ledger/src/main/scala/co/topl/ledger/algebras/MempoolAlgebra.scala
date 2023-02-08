package co.topl.ledger.algebras

import co.topl.{models => legacyModels}
import legacyModels.TypedIdentifier

trait MempoolAlgebra[F[_]] {

  /**
   * Read the set of unconfirmed Transaction IDs at the given block ID
   */
  def read(blockId: TypedIdentifier): F[Set[TypedIdentifier]]

  /**
   * Inserts an externally sourced Transaction ID into the Mempool
   */
  def add(transactionId: TypedIdentifier): F[Unit]

  /**
   * Remove/evict the given Transaction ID from the Mempool
   */
  def remove(transactionId: TypedIdentifier): F[Unit]

}
