package co.topl.modifier.block.serialization

import co.topl.attestation.serialization.{PublicKeyPropositionCurve25519Serializer, SignatureCurve25519Serializer}
import co.topl.attestation.{PublicKeyPropositionCurve25519, SignatureCurve25519}
import co.topl.modifier.ModifierId
import co.topl.modifier.block.{BlockHeader, BloomFilter}
import co.topl.nodeView.state.box.ArbitBox
import co.topl.nodeView.state.box.serialization.ArbitBoxSerializer
import co.topl.utils.serialization.{BifrostSerializer, Reader, Writer}
import scorex.crypto.hash.{Blake2b256, Digest32}

object BlockHeaderSerializer extends BifrostSerializer[BlockHeader] {
  override def serialize(header: BlockHeader, w: Writer): Unit = {
    /* version: Byte */
    w.put(header.version)

    /* blockId: ModifiedId */
    w.putBytes(header.id.hashBytes)

    /* parentId: ModifierId */
    w.putBytes(header.parentId.hashBytes)

    /* timestamp: Long */
    w.putULong(header.timestamp)

    /* generatorBox: ArbitBox */
    ArbitBoxSerializer.serialize(header.generatorBox, w)

    /* publicKey: PublicKeyCurve25519Proposition */
    PublicKeyPropositionCurve25519Serializer.serialize(header.publicKey, w)

    /* signature: Signature25519 */
    SignatureCurve25519Serializer.serialize(header.signature, w)

    /* height: Long */
    w.putLong(header.height)

    /* difficulty: Long */
    w.putLong(header.difficulty)

    /* txRoot: Array[Byte] */
    w.putBytes(header.txRoot)

    /* bloomFilter: Array[Long] */
    BloomFilter.serialize(header.bloomFilter, w)
  }

  override def parse(r: Reader): BlockHeader = {
    val version: Byte = r.getByte()

    val id: ModifierId = ModifierId(r.getBytes(ModifierId.size))

    val parentId: ModifierId = ModifierId(r.getBytes(ModifierId.size))

    val timestamp: Long = r.getULong()

    val generatorBox: ArbitBox = ArbitBoxSerializer.parse(r)

    val publicKey: PublicKeyPropositionCurve25519 = PublicKeyPropositionCurve25519Serializer.parse(r)

    val signature: SignatureCurve25519 = SignatureCurve25519Serializer.parse(r)

    val height: Long = r.getLong()

    val difficulty: Long = r.getLong()

    val txRoot: Digest32 = Digest32 @@ r.getBytes(Blake2b256.DigestSize)

    val bloomFilter: BloomFilter = BloomFilter.parse(r)

    BlockHeader(id, parentId, timestamp, generatorBox, publicKey, signature, height, difficulty, txRoot, bloomFilter, version)
  }
}
