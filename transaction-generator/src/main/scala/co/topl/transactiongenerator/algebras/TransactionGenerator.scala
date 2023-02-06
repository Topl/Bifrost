package co.topl.transactiongenerator.algebras

import co.topl.models.Transaction

/**
 * Responsible for the creation of (test/artificial) Transactions.  The Transactions should be valid,
 * and the order of the Transactions should be valid.
 * @tparam F an F-type constructor
 * @tparam G a collection/stream type
 */
trait TransactionGenerator[F[_], G[_]] {

  /**
   * Generate a collection `G` of transactions.  The returned collection may be unbounded.
   */
  def generateTransactions: F[G[Transaction]]
}
