package bifrost.modifier.transaction.bifrostTransaction

import bifrost.crypto.FastCryptographicHash
import bifrost.modifier.box.proposition.Proposition
import bifrost.nodeView.NodeViewModifier
import bifrost.nodeView.NodeViewModifier.{ModifierId, ModifierTypeId, bytesToId}


/**
  * A transaction is an atomic state modifier
  */

abstract class GenericTransaction[P <: Proposition] extends NodeViewModifier {
  override val modifierTypeId: ModifierTypeId = GenericTransaction.modifierTypeId

  val fee: Long

  val timestamp: Long

  val messageToSign: Array[Byte]

  override lazy val id: ModifierId = bytesToId(FastCryptographicHash(messageToSign))
}


object GenericTransaction {
  val modifierTypeId = NodeViewModifier.ModifierTypeId @@ (2: Byte)
  type TransactionId = ModifierId
}