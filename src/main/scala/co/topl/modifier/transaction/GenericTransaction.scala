package co.topl.modifier.transaction

import co.topl.crypto.FastCryptographicHash
import co.topl.modifier.ModifierId
import co.topl.nodeView.NodeViewModifier
import co.topl.nodeView.NodeViewModifier.ModifierTypeId
import co.topl.nodeView.box.proposition.Proposition


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