package bifrost.modifier.block

import bifrost.crypto.Signature25519
import bifrost.crypto.serialization.Signature25519Serializer
import bifrost.modifier.ModifierId
import bifrost.modifier.box.ArbitBox
import bifrost.modifier.box.serialization.BoxSerializer
import bifrost.modifier.transaction.bifrostTransaction.Transaction
import bifrost.modifier.transaction.serialization.TransactionSerializer
import bifrost.utils.Extensions._
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}

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
    w.putUInt(block.txs.length)

    /* txs: Seq[Transaction] */
    block.txs.foreach(tx => TransactionSerializer.serialize(tx, w))
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
