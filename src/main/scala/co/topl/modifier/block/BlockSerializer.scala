package co.topl.modifier.block

import co.topl.crypto.Signature25519
import co.topl.crypto.serialization.Signature25519Serializer
import co.topl.modifier.ModifierId
import co.topl.modifier.transaction.Transaction
import co.topl.modifier.transaction.serialization.TransactionSerializer
import co.topl.nodeView.state.box.ArbitBox
import co.topl.nodeView.state.box.serialization.BoxSerializer
import co.topl.utils.Extensions._
import co.topl.utils.serialization.{BifrostSerializer, Reader, Writer}

object BlockSerializer extends BifrostSerializer[Block] {

  override def serialize(block: Block, w: Writer): Unit = {
    /* version: Byte */
    w.put(block.version)

    /* parentId: ModifierId */
    w.putBytes(block.parentId.hashBytes)

    /* timestamp: Long */
    w.putULong(block.timestamp)

    /* generatorBox: ArbitBox */
    BoxSerializer.serialize(block.forgerBox, w)

    /* signature: Signature25519 */
    Signature25519Serializer.serialize(block.signature, w)

    /* txsLength: Int */
    w.putUInt(block.transactions.length)

    /* txs: Seq[Transaction] */
    block.transactions.foreach(tx => TransactionSerializer.serialize(tx, w))
  }

  override def parse(r: Reader): Block = {
    /* The order of the getByte, getLong... calls should not be changed */

    // TODO: Jing - Version should be used in the future to determine if we need additional procedures in parsing
    val version: Byte = r.getByte()

    // TODO: Jing - maybe we could check that the size of bytes to read in reader is less or equal to the max size of a block

    val parentId: ModifierId = ModifierId(r.getBytes(Block.blockIdLength))
    val timestamp: Long = r.getULong()

    // TODO: Jing - scorex uses toIntExact to make sure the Long does not exceed the length of an Int

    val generatorBox: ArbitBox = BoxSerializer.parse(r).asInstanceOf[ArbitBox]

    val signature: Signature25519 = Signature25519Serializer.parse(r)

    val txsLength: Int = r.getUInt().toIntExact
    val txs: Seq[Transaction] = (0 until txsLength).map(_ => TransactionSerializer.parse(r))

    Block(parentId, timestamp, generatorBox, signature, txs, version)
  }
}
