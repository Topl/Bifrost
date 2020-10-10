package co.topl.modifier.transaction

import co.topl.crypto.FastCryptographicHash
import co.topl.modifier.NodeViewModifier.ModifierTypeId
import co.topl.modifier.{ModifierId, NodeViewModifier}
import co.topl.nodeView.state.box.proposition.Proposition
import supertagged.@@


/**
  * A transaction is an atomic state modifier
  */
abstract class GenericTransaction[P <: Proposition] extends NodeViewModifier {

  override val modifierTypeId: ModifierTypeId = GenericTransaction.modifierTypeId

  val fee: Long

  val timestamp: Long

  val id: ModifierId = ModifierId(FastCryptographicHash(messageToSign))

  def messageToSign: Array[Byte]

}


object GenericTransaction {

  type TransactionId = ModifierId

  val modifierTypeId: Byte @@ NodeViewModifier.ModifierTypeId.Tag = NodeViewModifier.ModifierTypeId @@ (2: Byte)
}