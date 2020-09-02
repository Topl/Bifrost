package bifrost.nodeView

import bifrost.modifier.ModifierId
import bifrost.modifier.block.{Block, BlockSerializer}
import bifrost.modifier.transaction.bifrostTransaction.Transaction
import bifrost.modifier.transaction.serialization.TransactionSerializer
import bifrost.network.message.InvData
import bifrost.serialization.{BytesSerializable, JsonSerializable}
import bifrost.utils.serialization.BifrostSerializer
import bifrost.utils.{BifrostEncoder, BifrostEncoding}
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

  def bytesToId: Array[Byte] => ModifierId = bifrost.utils.bytesToId

  def idToBytes: ModifierId => Array[Byte] = bifrost.utils.idToBytes
}



trait PersistentNodeViewModifier extends NodeViewModifier {

  def parentId: ModifierId
}

trait TransactionsCarryingPersistentNodeViewModifier[TX <: Transaction]
  extends PersistentNodeViewModifier {

  def transactions: Seq[TX]
}
