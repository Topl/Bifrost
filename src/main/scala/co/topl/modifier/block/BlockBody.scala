package co.topl.modifier.block

import co.topl.modifier.NodeViewModifier
import co.topl.modifier.NodeViewModifier.ModifierTypeId
import co.topl.modifier.block.Block.BlockId
import co.topl.modifier.transaction.Transaction
import supertagged.@@

case class BlockBody(id: BlockId, parentId: BlockId, transactions: Seq[Transaction.TX])
  extends TransactionsCarryingPersistentNodeViewModifier[Transaction.TX] {

  override lazy val modifierTypeId: ModifierTypeId = BlockBody.modifierTypeId

}

object BlockBody {
  val modifierTypeId: Byte @@ NodeViewModifier.ModifierTypeId.Tag = ModifierTypeId @@ (4: Byte)
}