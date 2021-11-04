package co.topl.modifier

import co.topl.modifier.NodeViewModifier.ModifierTypeId
import co.topl.modifier.block.{Block, BlockBody, BlockHeader}
import co.topl.modifier.transaction.Transaction
import co.topl.utils.codecs.binary.legacy.modifier.NodeViewModifierSerializer
import co.topl.utils.codecs.binary.legacy.modifier.block.BlockSerializer
import co.topl.utils.codecs.binary.legacy.modifier.transaction.TransactionSerializer
import co.topl.utils.codecs.binary.legacy.{BifrostSerializer, BytesSerializable}
import io.circe.Encoder
import io.estatico.newtype.macros.newtype

import scala.language.implicitConversions

trait NodeViewModifier {
  lazy val serializer: BifrostSerializer[NodeViewModifier] = NodeViewModifierSerializer

  val modifierTypeId: ModifierTypeId

  def id: ModifierId
}

/**
 * It is supposed that all the modifiers (offchain transactions, blocks, blockheaders etc)
 * have identifiers of the some length fixed with the ModifierIdSize constant
 */
object NodeViewModifier {

  val modifierIdSize: Int = ModifierId.size // bytes (1 byte modifierTypeId + 32 modiifierId)

  @newtype
  case class ModifierTypeId(value: Byte)

  def idsToString(ids: Seq[(ModifierTypeId, ModifierId)]): String =
    List(ids.headOption, ids.lastOption).flatten
      .map { case (typeId, id) => s"($typeId,${id.toString})" }
      .mkString("[", "..", "]")

  def idsToString(modifierType: ModifierTypeId, ids: Seq[ModifierId]): String =
    idsToString(ids.map(id => (modifierType, id)))

  implicit val jsonEncoder: Encoder[NodeViewModifier] = {
    case mod: Block          => Block.jsonEncoder(mod)
    case mod: BlockHeader    => BlockHeader.jsonEncoder(mod)
    case mod: BlockBody      => BlockBody.jsonEncoder(mod)
    case mod: Transaction.TX => Transaction.jsonEncoder(mod)
    case other               => throw new Exception(s"Unknown modifier type: $other")
  }
}
