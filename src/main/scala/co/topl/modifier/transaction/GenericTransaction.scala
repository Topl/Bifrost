package co.topl.modifier.transaction

import co.topl.attestation.proof.Proof
import co.topl.attestation.proposition.Proposition
import co.topl.modifier.NodeViewModifier.ModifierTypeId
import co.topl.modifier.{ModifierId, NodeViewModifier}
import scorex.crypto.hash.Blake2b256
import supertagged.@@


/**
  * A transaction is an atomic state modifier
  */
abstract class GenericTransaction[P <: Proposition, PR <: Proof[P]] extends NodeViewModifier {

  val modifierTypeId: ModifierTypeId = GenericTransaction.modifierTypeId

  val fee: Long

  val timestamp: Long

  val attestation: Map[P, PR]

  val id: ModifierId = ModifierId(Blake2b256(messageToSign))

  def messageToSign: Array[Byte]

}


object GenericTransaction {

  type TransactionId = ModifierId

  val modifierTypeId: Byte @@ NodeViewModifier.ModifierTypeId.Tag = NodeViewModifier.ModifierTypeId @@ (2: Byte)
}