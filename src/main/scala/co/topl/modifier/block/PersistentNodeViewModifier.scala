package co.topl.modifier.block

import co.topl.modifier.block.PersistentNodeViewModifier.PNVMVersion
import co.topl.modifier.block.serialization.{BlockBodySerializer, BlockHeaderSerializer, BlockSerializer}
import co.topl.modifier.{ModifierId, NodeViewModifier}
import co.topl.utils.serialization.{BifrostSerializer, Reader, Writer}
import io.circe.Encoder

import scala.util.{Failure, Success}

trait PersistentNodeViewModifier extends NodeViewModifier {

  type M = PersistentNodeViewModifier
  lazy val serializer: BifrostSerializer[PersistentNodeViewModifier] = PersistentNodeViewModifier

  def parentId: ModifierId
  def version: PNVMVersion
}

object PersistentNodeViewModifier extends BifrostSerializer[PersistentNodeViewModifier] {
  type PNVMVersion = Byte

  override def serialize(obj: PersistentNodeViewModifier, w: Writer): Unit = {
    obj match {
      case obj: Block =>
        w.put(Block.modifierTypeId)
        BlockSerializer.serialize(obj, w)

      case obj: BlockHeader =>
        w.put(BlockHeader.modifierTypeId)
        BlockHeaderSerializer.serialize(obj, w)

      case obj: BlockBody =>
        w.put(BlockBody.modifierTypeId)
        BlockBodySerializer.serialize(obj, w)
    }
  }

  override def parse(r: Reader): PersistentNodeViewModifier = {
    (r.getByte() match {
      case Block.modifierTypeId       => BlockSerializer.parseTry(r)
      case BlockHeader.modifierTypeId => BlockHeaderSerializer.parseTry(r)
      case BlockBody.modifierTypeId   => BlockBodySerializer.parseTry(r)
    }) match {
      case Success(tx) => tx
      case Failure(ex) => throw ex
    }
  }

  implicit val jsonEncoder: Encoder[PersistentNodeViewModifier] = {
    case pmod: Block       => Block.jsonEncoder(pmod)
    case pmod: BlockHeader => BlockHeader.jsonEncoder(pmod)
    case pmod: BlockBody   => BlockBody.jsonEncoder(pmod)
    case other => throw new Exception(s"Unknown persistent modifier type: $other")
  }
}
