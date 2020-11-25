package co.topl.modifier.block.serialization

import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block.BlockId
import co.topl.modifier.block.{Block, BlockBody}
import co.topl.modifier.transaction.Transaction
import co.topl.modifier.transaction.serialization.TransactionSerializer
import co.topl.utils.Extensions.LongOps
import co.topl.utils.serialization.{BifrostSerializer, Reader, Writer}

object BlockBodySerializer extends BifrostSerializer[BlockBody] {
  override def serialize(body: BlockBody, w: Writer): Unit = {
    /* version: Byte */
    w.put(body.version)

    /* blockId: ModifiedId */
    w.putBytes(body.id.hashBytes)

    /* parentId: ModifierId */
    w.putBytes(body.parentId.hashBytes)

    /* txs: Seq[Transaction] */
    body.transactions.foreach(tx => TransactionSerializer.serialize(tx, w))
  }

  override def parse(r: Reader): BlockBody = {
    val version: Byte = r.getByte()

    val id: BlockId = ModifierId(r.getBytes(Block.blockIdLength))

    val parentId: ModifierId = ModifierId(r.getBytes(Block.blockIdLength))

    val txsLength: Int = r.getUInt().toIntExact
    val txs: Seq[Transaction.TX] = (0 until txsLength).map(_ => TransactionSerializer.parse(r))

    BlockBody(id, parentId, txs, version)
  }

}
