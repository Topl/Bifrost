package co.topl.nodeView.mempool

import co.topl.modifier.transaction.GenericTransaction
import co.topl.modifier.{ ContainsModifiers, ModifierId }
import co.topl.nodeView.NodeViewComponent

/**
  * Unconfirmed transactions pool
  *
  * @tparam TX -type of transaction the pool contains
  */
trait MemPoolReader[TX <: GenericTransaction[_]] extends NodeViewComponent with ContainsModifiers[TX] {

  //getters
  override def modifierById(modifierId: ModifierId): Option[TX]

  def contains(id: ModifierId): Boolean

  //get ids from Seq, not presenting in mempool
  def notIn(ids: Seq[ModifierId]): Seq[ModifierId] = ids.filter(id => !contains(id))

  def getAll(ids: Seq[ModifierId]): Seq[TX]

  def size: Int

  def take(limit: Int): Iterable[TX]

}
