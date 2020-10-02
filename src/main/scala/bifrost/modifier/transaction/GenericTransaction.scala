package bifrost.modifier.transaction

import bifrost.crypto.FastCryptographicHash
import bifrost.modifier.ModifierId
import bifrost.nodeView.NodeViewModifier
import bifrost.nodeView.NodeViewModifier.ModifierTypeId
import bifrost.nodeView.box.proposition.Proposition


/**
  * A transaction is an atomic state modifier
  */

abstract class GenericTransaction[P <: Proposition] extends NodeViewModifier {
  override val modifierTypeId: ModifierTypeId = GenericTransaction.modifierTypeId

  val fee: Long

  val timestamp: Long

  val messageToSign: Array[Byte]

  lazy val id: ModifierId = ModifierId(serializedId)

  lazy val serializedId: Array[Byte] = FastCryptographicHash(messageToSign)
}


object GenericTransaction {
  val modifierTypeId = NodeViewModifier.ModifierTypeId @@ (2: Byte)
  type TransactionId = ModifierId
}