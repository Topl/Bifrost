package co.topl.modifier

import co.topl.codecs.binary.legacy.modifier.NodeViewModifierSerializer
import co.topl.codecs.binary.legacy.{BifrostSerializer, BytesSerializable}
import co.topl.modifier.NodeViewModifier.ModifierTypeId
import io.estatico.newtype.macros.newtype

import scala.language.implicitConversions

trait NodeViewModifier extends BytesSerializable {

  type M = NodeViewModifier

  @deprecated
  override def serializer: BifrostSerializer[NodeViewModifier] = NodeViewModifierSerializer

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
}
