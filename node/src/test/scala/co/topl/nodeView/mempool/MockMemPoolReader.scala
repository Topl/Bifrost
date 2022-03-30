package co.topl.nodeView.mempool

import co.topl.modifier.ModifierId
import co.topl.modifier.transaction.Transaction

/**
 * A mock memory pool reader with an immutable set of transactions representing the current pool.
 * @param transactions the list of transactions that make up the memory pool
 * @tparam TX the type of transaction in the mem-pool
 */
class MockMemPoolReader[TX <: Transaction.TX](transactions: List[UnconfirmedTx[TX]]) extends MemPoolReader[TX] {
  override def modifierById(modifierId: ModifierId): Option[TX] = transactions.find(_.tx.id == modifierId).map(_.tx)

  override def getAll(ids: Seq[ModifierId]): Seq[TX] = transactions.filter(tx => ids.contains(tx.tx.id)).map(_.tx)

  override def size: Int = transactions.length

  override def take[A](limit: Int)(f: UnconfirmedTx[TX] => A)(implicit ord: Ordering[A]): Iterable[UnconfirmedTx[TX]] =
    transactions.map(tx => tx -> f(tx)).take(limit).sortBy(_._2).map(_._1)

  override type NVCT = this.type
}

object MockMemPoolReader {
  def apply[TX <: Transaction.TX](transactions: List[UnconfirmedTx[TX]]): MemPoolReader[TX] =
    new MockMemPoolReader[TX](transactions)
}
