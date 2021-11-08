package co.topl.codecs.binary.legacy.modifier.block

import co.topl.codecs.binary.legacy.attestation._
import co.topl.attestation.{PublicKeyPropositionCurve25519, SignatureCurve25519}
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.box.ArbitBox
import co.topl.modifier.transaction.Transaction
import co.topl.utils.Extensions.LongOps
import co.topl.codecs.binary.legacy.modifier.ModifierIdSerializer
import co.topl.codecs.binary.legacy.modifier.box.ArbitBoxSerializer
import co.topl.codecs.binary.legacy.modifier.transaction.TransactionSerializer
import co.topl.codecs.binary.legacy.{BifrostSerializer, Reader, Writer}

object BlockSerializer extends BifrostSerializer[Block] {

  override def serialize(block: Block, w: Writer): Unit = {
    /* version: Byte */
    w.put(block.version)

    /* parentId: ModifierId */
    ModifierIdSerializer.serialize(block.parentId, w)

    /* timestamp: Long */
    w.putULong(block.timestamp)

    /* generatorBox: ArbitBox */
    ArbitBoxSerializer.serialize(block.generatorBox, w)

    /* publicKey: PublicKeyCurve25519Proposition */
    PublicKeyPropositionCurve25519Serializer.serialize(block.publicKey, w)

    /* signature: Signature25519 */
    SignatureCurve25519Serializer.serialize(block.signature, w)

    /* height: Long */
    w.putULong(block.height)

    /* difficulty: Long */
    w.putULong(block.difficulty)

    /* txsLength: Int */
    w.putUInt(block.transactions.length)

    /* txs: Seq[Transaction] */
    block.transactions.foreach(tx => TransactionSerializer.serialize(tx, w))
  }

  override def parse(r: Reader): Block = {
    /* The order of the getByte, getLong... calls should not be changed */
    val version: Byte = r.getByte()

    val parentId: ModifierId = ModifierIdSerializer.parse(r)

    val timestamp: Long = r.getULong()

    val generatorBox: ArbitBox = ArbitBoxSerializer.parse(r)

    val publicKey: PublicKeyPropositionCurve25519 = PublicKeyPropositionCurve25519Serializer.parse(r)

    val signature: SignatureCurve25519 = SignatureCurve25519Serializer.parse(r)

    val height: Long = r.getULong()

    val difficulty: Long = r.getULong()

    val txsLength: Int = r.getUInt().toIntExact
    val txs: Seq[Transaction.TX] = for (_ <- 0 until txsLength) yield TransactionSerializer.parse(r)

    Block(parentId, timestamp, generatorBox, publicKey, signature, height, difficulty, txs, version)
  }
}
