package co.topl.codecs.json.modifier

import co.topl.modifier.NodeViewModifier
import co.topl.modifier.block.{Block, BlockBody, BlockHeader}
import co.topl.modifier.transaction.Transaction
import io.circe.Encoder

trait ModifierCodecs extends block.BlockCodecs with box.BoxCodecs with transaction.TransactionCodecs {

  implicit val nodeViewModifierJsonEncoder: Encoder[NodeViewModifier] = {
    case mod: Block          => blockJsonEncoder(mod)
    case mod: BlockHeader    => blockHeaderJsonEncoder(mod)
    case mod: BlockBody      => blockBodyJsonEncoder(mod)
    case mod: Transaction.TX => txJsonEncoder(mod)
    case other               => throw new Exception(s"Unknown modifier type: $other")
  }
}
