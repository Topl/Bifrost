package co.topl.nodeView

import co.topl.modifier.ModifierId
import co.topl.modifier.block.{ Block, BlockSerializer }
import co.topl.modifier.transaction.Transaction
import co.topl.modifier.transaction.serialization.TransactionSerializer
import co.topl.network.message.InvData
import co.topl.utils.serialization.{ BifrostSerializer, BytesSerializable, JsonSerializable }
import co.topl.utils.{ BifrostEncoder, BifrostEncoding }
import com.typesafe.config.ConfigFactory
import supertagged.TaggedType

import scala.util.Try

trait NodeViewModifier extends BytesSerializable with BifrostEncoding with JsonSerializable {
  self =>

  import NodeViewModifier.ModifierTypeId

  val modifierTypeId: ModifierTypeId

  //todo: check statically or dynamically output size
  def id: ModifierId

  def encodedId: String = encoder.encodeId(id)

  def serializedId: Array[Byte]
}

/**
  * It is supposed that all the modifiers (offchain transactions, blocks, blockheaders etc)
  * have identifiers of the some length fixed with the ModifierIdSize constant
  */
object NodeViewModifier {
  val DefaultIdSize = 32 // in bytes

  //TODO implement ModifierTypeId as a trait
  object ModifierTypeId extends TaggedType[Byte]
  type ModifierTypeId = ModifierTypeId.Type

  val ModifierIdSize: Int = Try(ConfigFactory.load().getConfig("app").getInt("modifierIdSize")).getOrElse(DefaultIdSize)

  val modifierSerializers: Map[ModifierTypeId, BifrostSerializer[_ <: NodeViewModifier]] =
    Map(
      Block.modifierTypeId -> BlockSerializer,
      Transaction.modifierTypeId -> TransactionSerializer
    )

  def idsToString(ids: Seq[(ModifierTypeId, ModifierId)])(implicit enc: BifrostEncoder): String = {
    List(ids.headOption, ids.lastOption)
      .flatten
      .map { case (typeId, id) => s"($typeId,${enc.encodeId(id)})" }
      .mkString("[", "..", "]")
  }

  def idsToString(modifierType: ModifierTypeId, ids: Seq[ModifierId])(implicit encoder: BifrostEncoder): String = {
    idsToString(ids.map(id => (modifierType, id)))
  }

  def idsToString(invData: InvData)(implicit encoder: BifrostEncoder): String = idsToString(invData.typeId, invData.ids)
}



trait PersistentNodeViewModifier extends NodeViewModifier {

  def parentId: ModifierId
}

trait TransactionsCarryingPersistentNodeViewModifier[TX <: Transaction]
  extends PersistentNodeViewModifier {

  def transactions: Seq[TX]
}
