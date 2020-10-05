package co.topl.modifier.block

import co.topl.modifier.transaction.Transaction
import co.topl.modifier.{ModifierId, NodeViewModifier}

trait PersistentNodeViewModifier extends NodeViewModifier {

  def parentId: ModifierId
}

trait TransactionsCarryingPersistentNodeViewModifier[TX <: Transaction] extends PersistentNodeViewModifier {

  def transactions: Seq[TX]
}