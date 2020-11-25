package co.topl.modifier

import co.topl.modifier.NodeViewModifier.ModifierTypeId
import co.topl.modifier.block.Block
import co.topl.modifier.block.serialization.BlockSerializer
import co.topl.modifier.transaction.Transaction
import co.topl.modifier.transaction.serialization.TransactionSerializer
import co.topl.network.message.InvData
import co.topl.utils.serialization.{BifrostSerializer, BytesSerializable}
import supertagged.TaggedType

trait NodeViewModifier extends BytesSerializable {
  val modifierTypeId: ModifierTypeId

  def id: ModifierId
}

/**
  * It is supposed that all the modifiers (offchain transactions, blocks, blockheaders etc)
  * have identifiers of the some length fixed with the ModifierIdSize constant
  */
object NodeViewModifier {
  val ModifierIdSize = 32 // bytes

  object ModifierTypeId extends TaggedType[Byte]
  type ModifierTypeId = ModifierTypeId.Type

  val modifierSerializers: Map[ModifierTypeId, BifrostSerializer[_ <: NodeViewModifier]] =
    Map(Block.modifierTypeId -> BlockSerializer, Transaction.modifierTypeId -> TransactionSerializer)

  def idsToString(ids: Seq[(ModifierTypeId, ModifierId)]): String = {
    List(ids.headOption, ids.lastOption)
      .flatten
      .map { case (typeId, id) => s"($typeId,${id.toString})" }
      .mkString("[", "..", "]")
  }

  def idsToString(modifierType: ModifierTypeId, ids: Seq[ModifierId]): String = {
    idsToString(ids.map(id => (modifierType, id)))
  }

  def idsToString(invData: InvData): String = idsToString(invData.typeId, invData.ids)
}