package co.topl.nodeView.mempool

import co.topl.attestation.EvidenceProducer
import co.topl.attestation.proof.Proof
import co.topl.attestation.proposition.Proposition
import co.topl.modifier.transaction.Transaction
import co.topl.modifier.{ ContainsModifiers, ModifierId }
import co.topl.nodeView.NodeViewComponent
import co.topl.nodeView.state.box.Box

/**
  * Unconfirmed transactions pool
  *
  * @tparam TX -type of transaction the pool contains
  */
trait MemPoolReader extends NodeViewComponent with ContainsModifiers[Transaction[_, Proposition, Proof[_], Box[_]]] {

  type TX = Transaction[_, Proposition, Proof[_], Box[_]]

  //getters
  override def modifierById(modifierId: ModifierId): Option[TX]

  def contains(id: ModifierId): Boolean

  //get ids from Seq, not presenting in mempool
  def notIn(ids: Seq[ModifierId]): Seq[ModifierId] = ids.filter(id => !contains(id))

  def getAll(ids: Seq[ModifierId]): Seq[TX]

  def size: Int

  def take[P: Proposition: EvidenceProducer, T: Transaction[_, P, Proof[P], Box[_]]] ( limit: Int): Iterable[T]

}
