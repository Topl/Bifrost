package co.topl.genus.orientDb.instances

import co.topl.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import co.topl.consensus.models._
import co.topl.codecs.bytes.tetra.TetraScodecCodecs._
import co.topl.codecs.bytes.typeclasses.ImmutableCodec
import co.topl.genus.orientDb.schema.OIndexable.Instances
import co.topl.genus.orientDb.schema.{GraphDataEncoder, OIndexable, VertexSchema}
import co.topl.genus.orientDb.schema.OTyped.Instances._
import com.google.protobuf.ByteString

object SchemaBlockHeader {
  val SchemaName = "BlockHeader"

  /**
   * BlockHeader model fields:
   *
   * @see https://github.com/Topl/protobuf-specs/blob/main/proto/consensus/models/block_header.proto
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
    val Size = "size"
    val Version = "version"
    val BlockHeaderIndex = "blockHeaderIndex"
    val BlockHeaderHeightIndex = "blockHeaderHeightIndex"
  }

  def size(blockHeader: BlockHeader): Long =
    ImmutableCodec.fromScodecCodec[BlockHeader].immutableBytes(blockHeader).size

  def make(): VertexSchema[BlockHeader] = VertexSchema.create(
    SchemaName,
    GraphDataEncoder[BlockHeader]
      // @formatter:off
      .withProperty(Field.BlockId, _.id.value.toByteArray,  mandatory = true, readOnly = true, notNull = true)
      .withProperty(Field.ParentHeaderId, _.parentHeaderId.value.toByteArray,  mandatory = true, readOnly = true, notNull = true)
      .withProperty(Field.ParentSlot, l => java.lang.Long.valueOf(l.parentSlot),  mandatory = false, readOnly = true, notNull = false)
      .withProperty(Field.TxRoot, _.txRoot.toByteArray, mandatory = false, readOnly = true, notNull = false)
      .withProperty(Field.BloomFilter, _.bloomFilter.toByteArray,   mandatory = false, readOnly = true, notNull = false)
      .withProperty(Field.Timestamp, ts => java.lang.Long.valueOf(ts.timestamp),mandatory = true, readOnly = true, notNull = true)
      .withProperty(Field.Height, ht => java.lang.Long.valueOf(ht.height),mandatory = true, readOnly = true, notNull = true)
      .withProperty(Field.Slot, s => java.lang.Long.valueOf(s.slot),mandatory = true, readOnly = true, notNull = true)
      .withProperty(Field.EligibilityCertificate, e => e.eligibilityCertificate.toByteArray,mandatory = true, readOnly = true, notNull = true)
      .withProperty(Field.OperationalCertificate,_.operationalCertificate.toByteArray,mandatory = true, readOnly = true, notNull = true)
      .withProperty(Field.Metadata,_.metadata.toByteArray,mandatory = true, readOnly = true, notNull = false)
      .withProperty(Field.Address,_.address.toByteArray,mandatory = true, readOnly = true, notNull = true)
      .withProperty(Field.Size, blockHeader => java.lang.Long.valueOf(size(blockHeader)) ,mandatory = true, readOnly = true, notNull = true)
      .withProperty(Field.Version, blockHeader => blockHeader.version.toByteArray ,mandatory = true, readOnly = true, notNull = true)
      .withIndex[BlockHeader](Field.BlockHeaderIndex, Field.BlockId)(Instances.blockHeader)
      .withIndex[BlockHeader](Field.BlockHeaderHeightIndex, Field.Height)(OIndexable.Instances.blockHeightHeader),
      // @formatter:on
    v =>
      BlockHeader(
        Option(BlockId(ByteString.copyFrom(v(Field.BlockId): Array[Byte]))),
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
        address = StakingAddress.parseFrom(v(Field.Address): Array[Byte]),
        version = ProtocolVersion.parseFrom(v(Field.Version): Array[Byte])
      )
  )

}
