package co.topl.modifier.block.serialization

import co.topl.attestation.serialization.{PublicKeyPropositionCurve25519Serializer, SignatureCurve25519Serializer}
import co.topl.attestation.{PublicKeyPropositionCurve25519, SignatureCurve25519}
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.transaction.Transaction
import co.topl.modifier.transaction.serialization.TransactionSerializer
import co.topl.nodeView.state.box.ArbitBox
import co.topl.nodeView.state.box.serialization.ArbitBoxSerializer
import co.topl.utils.Extensions.LongOps
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
    ArbitBoxSerializer.serialize(block.generatorBox, w)

    /* publicKey: PublicKeyCurve25519Proposition */
    PublicKeyPropositionCurve25519Serializer.serialize(block.publicKey, w)

    /* signature: Signature25519 */
    SignatureCurve25519Serializer.serialize(block.signature, w)

    /* height: Long */
    w.putLong(block.height)

    /* difficulty: Long */
    w.putLong(block.difficulty)

    /* txsLength: Int */
    w.putUInt(block.transactions.length)

    /* txs: Seq[Transaction] */
    block.transactions.foreach(tx => TransactionSerializer.serialize(tx, w))
  }

  override def parse(r: Reader): Block = {
    /* The order of the getByte, getLong... calls should not be changed */
    val version: Byte = r.getByte()

    val parentId: ModifierId = ModifierId(r.getBytes(Block.blockIdLength))

    val timestamp: Long = r.getULong()

    val generatorBox: ArbitBox = ArbitBoxSerializer.parse(r)

    val publicKey: PublicKeyPropositionCurve25519 = PublicKeyPropositionCurve25519Serializer.parse(r)

    val signature: SignatureCurve25519 = SignatureCurve25519Serializer.parse(r)

    val height: Long = r.getLong()

    val difficulty: Long = r.getLong()

    val txsLength: Int = r.getUInt().toIntExact
    val txs: Seq[Transaction.TX] = (0 until txsLength).map(_ => TransactionSerializer.parse(r))

    Block(parentId, timestamp, generatorBox, publicKey, signature, height, difficulty, txs, version)
  }
}
