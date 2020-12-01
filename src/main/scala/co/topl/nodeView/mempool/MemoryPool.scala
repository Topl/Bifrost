package co.topl.nodeView.mempool

import co.topl.modifier.ModifierId
import co.topl.modifier.transaction.GenericTransaction
import co.topl.nodeView.NodeViewComponent

import scala.util.Try

/**
  * Unconfirmed transactions pool
  *
  * @tparam TX -type of transaction the pool contains
  */
trait MemoryPool[TX <: GenericTransaction[_], M <: MemoryPool[TX, M]] extends NodeViewComponent with MemPoolReader[TX] {

  //getters
  def modifierById(id: ModifierId): Option[TX]

  def contains(id: ModifierId): Boolean

  //get ids from Seq, not presenting in mempool
  override def notIn(ids: Seq[ModifierId]): Seq[ModifierId] = ids.filter(id => !contains(id))

  def getAll(ids: Seq[ModifierId]): Seq[TX]

  //modifiers
  def put(tx: TX): Try[M]

  def put(txs: Iterable[TX]): Try[M]

  def putWithoutCheck(txs: Iterable[TX]): M

  def remove(tx: TX): M

  def take(limit: Int): Iterable[TX]

  def filter(txs: Seq[TX]): M = filter(t => !txs.exists(_.id == t.id))

  def filter(condition: TX => Boolean): M

  def size: Int

  /**
    * @return read-only copy of this state
    */
  def getReader: MemPoolReader[TX] = this
}
