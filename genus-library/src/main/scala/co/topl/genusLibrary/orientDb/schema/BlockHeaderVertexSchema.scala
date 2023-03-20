package co.topl.genusLibrary.orientDb.schema

import co.topl.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import co.topl.consensus.models._
import co.topl.genusLibrary.orientDb.schema.OrientDbIndexable.Instances._
import co.topl.genusLibrary.orientDb.schema.OrientDbTyped.Instances._
import com.google.protobuf.ByteString

object BlockHeaderVertexSchema {

  /**
   * Names should be aligned with BlockHeader model fields:
   * @see https://github.com/Topl/protobuf-specs/blob/main/proto/consensus/models/block_header.proto#L15
   */
  object Field {
    val BlockId = "blockId"
    val ParentHeaderId = "parentHeaderId"
    val ParentSlot = "parentSlot"
    val TxRoot = "txRoot"
    val BloomFilter = "bloomFilter"
    val Timestamp = "timestamp"
    val Height = "height"
    val Slot = "slot"
    val EligibilityCertificate = "eligibilityCertificate"
    val OperationalCertificate = "operationalCertificate"
    val Metadata = "metadata"
    val Address = "address"
    val BlockHeaderIndex = "blockHeaderIndex"
  }

  def make(): VertexSchema[BlockHeader] = VertexSchema.create(
    "BlockHeader",
    GraphDataEncoder[BlockHeader]
      // @formatter:off
      .withProperty(Field.BlockId, _.id.value.toByteArray, _.setNotNull(true).setReadonly(true).setMandatory(true))
      .withProperty(Field.ParentHeaderId, _.parentHeaderId.value.toByteArray, _.setNotNull(true).setReadonly(true).setMandatory(true))
      .withProperty(Field.ParentSlot, l => java.lang.Long.valueOf(l.parentSlot), _.setReadonly(true))
      .withProperty(Field.TxRoot, _.txRoot.toByteArray, _.setReadonly(true))
      .withProperty(Field.BloomFilter, _.bloomFilter.toByteArray, _.setReadonly(true))
      .withProperty(Field.Timestamp, ts => java.lang.Long.valueOf(ts.timestamp),_.setNotNull(true).setReadonly(true).setMandatory(true))
      .withProperty(Field.Height, ht => java.lang.Long.valueOf(ht.height),_.setNotNull(true).setReadonly(true).setMandatory(true))
      .withProperty(Field.Slot, s => java.lang.Long.valueOf(s.slot),_.setNotNull(true).setReadonly(true).setMandatory(true))
      .withProperty(Field.EligibilityCertificate, e => e.eligibilityCertificate.toByteArray,_.setNotNull(true).setReadonly(true).setMandatory(true))
      .withProperty(Field.OperationalCertificate,_.operationalCertificate.toByteArray,_.setNotNull(true).setReadonly(true).setMandatory(true))
      .withProperty(Field.Metadata,_.metadata.toByteArray,_.setNotNull(false).setReadonly(true).setMandatory(true))
      .withProperty(Field.Address,_.address.toByteArray,_.setNotNull(true).setReadonly(true).setMandatory(true))
      .withIndex(Field.BlockHeaderIndex, Field.BlockId),
      // @formatter:on
    v =>
      BlockHeader(
        BlockId(ByteString.copyFrom(v(Field.ParentHeaderId): Array[Byte])),
        v(Field.ParentSlot),
        ByteString.copyFrom(v(Field.TxRoot): Array[Byte]),
        ByteString.copyFrom(v(Field.BloomFilter): Array[Byte]),
        v(Field.Timestamp),
        v(Field.Height),
        v(Field.Slot),
        eligibilityCertificate = EligibilityCertificate.parseFrom(v(Field.EligibilityCertificate): Array[Byte]),
        operationalCertificate = OperationalCertificate.parseFrom(v(Field.OperationalCertificate): Array[Byte]),
        ByteString.copyFrom(v(Field.Metadata): Array[Byte]),
        address = StakingAddress.parseFrom(v(Field.Address): Array[Byte])
      )
  )

}
