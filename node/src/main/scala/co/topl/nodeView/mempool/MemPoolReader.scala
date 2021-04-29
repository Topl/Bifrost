package co.topl.nodeView.mempool

import co.topl.modifier.transaction.Transaction
import co.topl.modifier.{ContainsModifiers, ModifierId}
import co.topl.nodeView.NodeViewComponent
import co.topl.utils.TimeProvider

import scala.math.Ordering

case class UnconfirmedTx[TX <: Transaction.TX](tx: TX, dateAdded: TimeProvider.Time)

/**
 * Unconfirmed transactions pool
 */
trait MemPoolReader[TX <: Transaction.TX] extends NodeViewComponent with ContainsModifiers[TX] {

  //getters
  override def modifierById(modifierId: ModifierId): Option[TX]

  def contains(id: ModifierId): Boolean

  //get ids from Seq, not presenting in mempool
  def notIn(ids: Seq[ModifierId]): Seq[ModifierId] = ids.filter(id => !contains(id))

  def getAll(ids: Seq[ModifierId]): Seq[TX]

  def size: Int

  def take[A](limit: Int)(f: UnconfirmedTx[TX] => A)(implicit ord: Ordering[A]): Iterable[UnconfirmedTx[TX]]

}
