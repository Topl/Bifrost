package co.topl.modifier.block.serialization

import co.topl.modifier.ModifierId
import co.topl.modifier.block.BlockBody
import co.topl.modifier.transaction.Transaction
import co.topl.modifier.transaction.serialization.TransactionSerializer
import co.topl.utils.Extensions.LongOps
import co.topl.utils.serialization.{BifrostSerializer, Reader, Writer}

object BlockBodySerializer extends BifrostSerializer[BlockBody] {

  override def serialize(body: BlockBody, w: Writer): Unit = {
    /* version: Byte */
    w.put(body.version)

    /* blockId: ModifiedId */
    ModifierId.serialize(body.id, w)

    /* parentId: ModifierId */
    ModifierId.serialize(body.parentId, w)

    /* txs: Seq[Transaction] */
    w.putUInt(body.transactions.size)
    body.transactions.foreach(tx => TransactionSerializer.serialize(tx, w))
  }

  override def parse(r: Reader): BlockBody = {
    val version: Byte = r.getByte()

    val id: ModifierId = ModifierId.parse(r)

    val parentId: ModifierId = ModifierId.parse(r)

    val txsLength: Int = r.getUInt().toIntExact
    val txs: Seq[Transaction.TX] = for (_ <- 0 until txsLength) yield TransactionSerializer.parse(r)

    BlockBody(id, parentId, txs, version)
  }

}
