package bifrost.modifier.transaction.bifrostTransaction

import bifrost.nodeView.NodeViewModifier._
import bifrost.crypto.FastCryptographicHash
import bifrost.modifier.box.proposition.Proposition
import bifrost.nodeView.NodeViewModifier


/**
  * A transaction is an atomic state modifier
  */

abstract class GenericTransaction[P <: Proposition] extends NodeViewModifier {
  override val modifierTypeId: Byte = GenericTransaction.ModifierTypeId

  val fee: Long

  val timestamp: Long

  val messageToSign: Array[Byte]

  override lazy val id: ModifierId = FastCryptographicHash(messageToSign)
}


object GenericTransaction {
  val ModifierTypeId = 2: Byte
  type TransactionId = NodeViewModifier.ModifierId
}