package co.topl.genus.algebras

import co.topl.genus.filters.{BlockFilter, TransactionFilter}
import co.topl.genus.types.{Block, Transaction}

/** Subscribes and queries both blocks and transactions maintained in some data store.
  * @tparam F context type wrapping responses
  * @tparam G collection type of responses
  */
trait DatabaseClientAlg[F[_], G[_]] {
  def subscribeToTransactions(filter: TransactionFilter): F[G[Transaction]]
  def subscribeToBlocks(filter: BlockFilter): F[G[Block]]
  def queryTransactions(filter: TransactionFilter): F[G[Transaction]]
  def queryBlocks(filter: BlockFilter): F[G[Block]]
}
