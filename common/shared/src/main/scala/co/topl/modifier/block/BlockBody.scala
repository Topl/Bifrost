package co.topl.modifier.block

import co.topl.modifier.NodeViewModifier.ModifierTypeId
import co.topl.modifier.block.PersistentNodeViewModifier.PNVMVersion
import co.topl.modifier.transaction.Transaction
import co.topl.modifier.{ModifierId, NodeViewModifier}

case class BlockBody(id: ModifierId, parentId: ModifierId, transactions: Seq[Transaction.TX], version: PNVMVersion)
    extends TransactionCarryingPersistentNodeViewModifier[Transaction.TX] {

  override lazy val modifierTypeId: ModifierTypeId = BlockBody.modifierTypeId

}

object BlockBody {

  val modifierTypeId: NodeViewModifier.ModifierTypeId = ModifierTypeId(5: Byte)
}
