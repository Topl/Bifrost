package co.topl.codecs.binary.legacy.modifier.block

import co.topl.codecs.binary.legacy.modifier.ModifierIdSerializer
import co.topl.codecs.binary.legacy.modifier.transaction.TransactionSerializer
import co.topl.codecs.binary.legacy.{BifrostSerializer, Reader, Writer}
import co.topl.modifier.ModifierId
import co.topl.modifier.block.BlockBody
import co.topl.modifier.transaction.Transaction
import co.topl.utils.Extensions.LongOps

object BlockBodySerializer extends BifrostSerializer[BlockBody] {

  override def serialize(body: BlockBody, w: Writer): Unit = {
    /* version: Byte */
    w.put(body.version)

    /* blockId: ModifiedId */
    ModifierIdSerializer.serialize(body.id, w)

    /* parentId: ModifierId */
    ModifierIdSerializer.serialize(body.parentId, w)

    /* txs: Seq[Transaction] */
    w.putUInt(body.transactions.size)
    body.transactions.foreach(tx => TransactionSerializer.serialize(tx, w))
  }

  override def parse(r: Reader): BlockBody = {
    val version: Byte = r.getByte()

    val id: ModifierId = ModifierIdSerializer.parse(r)

    val parentId: ModifierId = ModifierIdSerializer.parse(r)

    val txsLength: Int = r.getUInt().toIntExact
    val txs: Seq[Transaction.TX] = for (_ <- 0 until txsLength) yield TransactionSerializer.parse(r)

    BlockBody(id, parentId, txs, version)
  }

}
