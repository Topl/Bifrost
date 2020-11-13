package co.topl.modifier.block

import co.topl.attestation.proof.{Proof, SignatureCurve25519, SignatureCurve25519Serializer}
import co.topl.attestation.proposition.{Proposition, PublicKeyPropositionCurve25519, PublicKeyPropositionCurve25519Serializer}
import co.topl.modifier.ModifierId
import co.topl.modifier.transaction.Transaction
import co.topl.modifier.transaction.serialization.TransactionSerializer
import co.topl.nodeView.state.box.{ArbitBox, Box}
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

    /* publicKey: PublicKeyCurve25519Proposition */
    PublicKeyPropositionCurve25519Serializer.serialize(block.publicKey, w)

    /* signature: Signature25519 */
    SignatureCurve25519Serializer.serialize(block.signature, w)

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

    val publicKey: PublicKeyPropositionCurve25519 = PublicKeyPropositionCurve25519Serializer.parse(r)

    val signature: SignatureCurve25519 = SignatureCurve25519Serializer.parse(r)

    val txsLength: Int = r.getUInt().toIntExact
    val txs: Seq[Transaction[_, _ <: Proposition, _ <: Proof[_], _ <: Box[_]]] = (0 until txsLength).map(_ => TransactionSerializer.parse(r))

    Block(parentId, timestamp, generatorBox, publicKey, signature, txs, version)
  }
}
