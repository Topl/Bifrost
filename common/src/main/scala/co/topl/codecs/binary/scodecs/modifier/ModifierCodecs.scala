package co.topl.codecs.binary.scodecs.modifier

import co.topl.codecs.binary.scodecs.valuetypes._
import co.topl.modifier.NodeViewModifier
import co.topl.modifier.block.{Block, BlockBody, BlockHeader}
import co.topl.modifier.transaction.Transaction
import scodec.Codec
import scodec.codecs.discriminated

trait ModifierCodecs extends box.BoxCodecs with transaction.TransactionCodecs with block.BlockCodecs {

  implicit val nodeViewModifierCodec: Codec[NodeViewModifier] =
    discriminated[NodeViewModifier]
      .by(byteCodec)
      .typecase(Block.modifierTypeId.value, blockCodec)
      .typecase(BlockHeader.modifierTypeId.value, blockHeaderCodec)
      .typecase(BlockBody.modifierTypeId.value, blockBodyCodec)
      .typecase(Transaction.modifierTypeId.value, transactionCodec)
}
