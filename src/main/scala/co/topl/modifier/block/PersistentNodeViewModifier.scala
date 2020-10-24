package co.topl.modifier.block

import co.topl.modifier.transaction.Transaction
import co.topl.modifier.{ ModifierId, NodeViewModifier }
import io.circe.Encoder

sealed trait PersistentNodeViewModifier extends NodeViewModifier {

  def parentId: ModifierId
}

trait TransactionsCarryingPersistentNodeViewModifier[TX <: Transaction] extends PersistentNodeViewModifier {

  def transactions: Seq[TX]
}

object PersistentNodeViewModifier {
  implicit val jsonEncoder: Encoder[PersistentNodeViewModifier] = {
    case b: Block ⇒ Block.jsonEncoder(b)
    case other => throw new Exception(s"Unknown persistent modifier type: $other")
  }
}