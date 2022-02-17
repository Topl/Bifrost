package co.topl.codecs.binary.legacy.modifier.block

import co.topl.attestation.{PublicKeyPropositionCurve25519, SignatureCurve25519}
import co.topl.codecs.binary.legacy.attestation._
import co.topl.codecs.binary.legacy.modifier.ModifierIdSerializer
import co.topl.codecs.binary.legacy.modifier.box.ArbitBoxSerializer
import co.topl.codecs.binary.legacy.{BifrostSerializer, Reader, Writer}
import co.topl.crypto.hash.digest.Digest32
import co.topl.modifier.ModifierId
import co.topl.modifier.block.{BlockHeader, BloomFilter}
import co.topl.modifier.box.ArbitBox
import co.topl.utils.IdiomaticScalaTransition.implicits.toValidatedOps

object BlockHeaderSerializer extends BifrostSerializer[BlockHeader] {

  override def serialize(header: BlockHeader, w: Writer): Unit = {
    /* version: Byte */
    w.put(header.version)

    /* blockId: ModifiedId */
    ModifierIdSerializer.serialize(header.id, w)

    /* parentId: ModifierId */
    ModifierIdSerializer.serialize(header.parentId, w)

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
    w.putBytes(header.txRoot.value)

    /* bloomFilter: Array[Long] */
    BloomFilterSerializer.serialize(header.bloomFilter, w)
  }

  override def parse(r: Reader): BlockHeader = {
    val version: Byte = r.getByte()

    val id: ModifierId = ModifierIdSerializer.parse(r)

    val parentId: ModifierId = ModifierIdSerializer.parse(r)

    val timestamp: Long = r.getULong()

    val generatorBox: ArbitBox = ArbitBoxSerializer.parse(r)

    val publicKey: PublicKeyPropositionCurve25519 = PublicKeyPropositionCurve25519Serializer.parse(r)

    val signature: SignatureCurve25519 = SignatureCurve25519Serializer.parse(r)

    val height: Long = r.getLong()

    val difficulty: Long = r.getLong()

    val txRoot: Digest32 =
      Digest32
        .validated(r.getBytes(Digest32.size))
        .getOrThrow()

    val bloomFilter: BloomFilter = BloomFilterSerializer.parse(r)

    BlockHeader(
      id,
      parentId,
      timestamp,
      generatorBox,
      publicKey,
      signature,
      height,
      difficulty,
      txRoot,
      bloomFilter,
      version
    )
  }
}
