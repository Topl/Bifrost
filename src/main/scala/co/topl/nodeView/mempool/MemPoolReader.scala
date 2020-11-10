package co.topl.nodeView.mempool

import co.topl.attestation.proof.Proof
import co.topl.attestation.proposition.Proposition
import co.topl.modifier.transaction.Transaction
import co.topl.modifier.{ ContainsModifiers, ModifierId }
import co.topl.nodeView.NodeViewComponent
import co.topl.nodeView.state.box.Box

/**
  * Unconfirmed transactions pool
  */
trait MemPoolReader[TX <: Transaction[_, Proposition, Proof[_], Box[_]]]
  extends NodeViewComponent with ContainsModifiers[TX] {

  //getters
  override def modifierById(modifierId: ModifierId): Option[TX]

  def contains(id: ModifierId): Boolean

  //get ids from Seq, not presenting in mempool
  def notIn(ids: Seq[ModifierId]): Seq[ModifierId] = ids.filter(id => !contains(id))

  def getAll(ids: Seq[ModifierId]): Seq[TX]

  def size: Int

  def take( limit: Int): Iterable[TX]

}
