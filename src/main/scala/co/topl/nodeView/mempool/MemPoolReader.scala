package co.topl.nodeView.mempool

import co.topl.modifier.{ContainsModifiers, ModifierId, NodeViewModifier}
import co.topl.nodeView.NodeViewComponent

/**
  * Unconfirmed transactions pool
  */
trait MemPoolReader[TX <: NodeViewModifier]
  extends NodeViewComponent with ContainsModifiers[TX] {

  //getters
  override def modifierById(modifierId: ModifierId): Option[TX]

  def contains(id: ModifierId): Boolean

  //get ids from Seq, not presenting in mempool
  def notIn(ids: Seq[ModifierId]): Seq[ModifierId] = ids.filter(id => !contains(id))

  def getAll(ids: Seq[ModifierId]): Seq[TX]

  def size: Int

  def take(limit: Int): Iterable[TX]

}
