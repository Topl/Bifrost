package co.topl.utils.codecs.binary.legacy.modifier

import co.topl.modifier.NodeViewModifier
import co.topl.modifier.block.{Block, BlockBody, BlockHeader}
import co.topl.modifier.transaction.Transaction
import co.topl.utils.codecs.binary.legacy.modifier.block.{BlockBodySerializer, BlockHeaderSerializer, BlockSerializer}
import co.topl.utils.codecs.binary.legacy.modifier.transaction.TransactionSerializer
import co.topl.utils.codecs.binary.legacy.{BifrostSerializer, Reader, Writer}

import scala.util.{Failure, Success}

object NodeViewModifierSerializer extends BifrostSerializer[NodeViewModifier] {

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
}
