package co.topl.modifier

import co.topl.modifier.NodeViewModifier.ModifierTypeId
import co.topl.modifier.block.serialization.{BlockBodySerializer, BlockHeaderSerializer, BlockSerializer}
import co.topl.modifier.block.{Block, BlockBody, BlockHeader}
import co.topl.modifier.transaction.Transaction
import co.topl.modifier.transaction.serialization.TransactionSerializer
import co.topl.utils.serialization.{BifrostSerializer, BytesSerializable, Reader, Writer}
import io.circe.Encoder
import io.estatico.newtype.macros.newtype

import scala.Predef.->
import scala.language.implicitConversions
import scala.util.{Failure, Success}

trait NodeViewModifier extends BytesSerializable {
  type M = NodeViewModifier
  lazy val serializer: BifrostSerializer[NodeViewModifier] = NodeViewModifier

  val modifierTypeId: ModifierTypeId

  def id: ModifierId
}

/**
 * It is supposed that all the modifiers (offchain transactions, blocks, blockheaders etc)
 * have identifiers of the some length fixed with the ModifierIdSize constant
 */
object NodeViewModifier extends BifrostSerializer[NodeViewModifier] {

  val modifierIdSize: Int = ModifierId.size // bytes (1 byte modifierTypeId + 32 modiifierId)

  @newtype
  case class ModifierTypeId(value: Byte)

  val modifierSerializers: Map[ModifierTypeId, BifrostSerializer[_ <: NodeViewModifier]] =
    Map(Block.modifierTypeId -> BlockSerializer, Transaction.modifierTypeId -> TransactionSerializer)

  def idsToString(ids: Seq[(ModifierTypeId, ModifierId)]): String =
    List(ids.headOption, ids.lastOption).flatten
      .map { case (typeId, id) => s"($typeId,${id.toString})" }
      .mkString("[", "..", "]")

  def idsToString(modifierType: ModifierTypeId, ids: Seq[ModifierId]): String =
    idsToString(ids.map(id => (modifierType, id)))

  override def serialize(obj: NodeViewModifier, w: Writer): Unit =
    obj match {
      case obj: Block =>
        w.put(Block.modifierTypeId.value)
        BlockSerializer.serialize(obj, w)

      case obj: BlockHeader =>
        w.put(BlockHeader.modifierTypeId.value)
        BlockHeaderSerializer.serialize(obj, w)

      case obj: BlockBody =>
        w.put(BlockBody.modifierTypeId.value)
        BlockBodySerializer.serialize(obj, w)

      case obj: Transaction.TX =>
        w.put(Transaction.modifierTypeId.value)
        TransactionSerializer.serialize(obj, w)
    }

  override def parse(r: Reader): NodeViewModifier =
    (r.getByte() match {
      case b if b == Block.modifierTypeId.value       => BlockSerializer.parseTry(r)
      case b if b == BlockHeader.modifierTypeId.value => BlockHeaderSerializer.parseTry(r)
      case b if b == BlockBody.modifierTypeId.value   => BlockBodySerializer.parseTry(r)
      case b if b == Transaction.modifierTypeId.value => TransactionSerializer.parseTry(r)
    }) match {
      case Success(tx) => tx
      case Failure(ex) => throw ex
    }

  implicit val jsonEncoder: Encoder[NodeViewModifier] = {
    case mod: Block          => Block.jsonEncoder(mod)
    case mod: BlockHeader    => BlockHeader.jsonEncoder(mod)
    case mod: BlockBody      => BlockBody.jsonEncoder(mod)
    case mod: Transaction.TX => Transaction.jsonEncoder(mod)
    case other               => throw new Exception(s"Unknown modifier type: $other")
  }
}
