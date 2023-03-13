package co.topl.genusLibrary.model

import co.topl.consensus.models.BlockHeader
import co.topl.genusLibrary.orientDb.GenusGraphMetadata.getBlockId
import co.topl.consensus.models._
import com.google.protobuf.ByteString

/**
 * A block Header with Block Id
 */
final case class BlockHeaderWrapper(
  blockId:                BlockId,
  parentHeaderId:         BlockId,
  parentSlot:             _root_.scala.Long = 0L,
  txRoot:                 ByteString = ByteString.EMPTY,
  bloomFilter:            ByteString = ByteString.EMPTY,
  timestamp:              _root_.scala.Long = 0L,
  height:                 _root_.scala.Long = 0L,
  slot:                   _root_.scala.Long = 0L,
  eligibilityCertificate: EligibilityCertificate,
  operationalCertificate: OperationalCertificate,
  metadata:               ByteString = ByteString.EMPTY,
  address:                ByteString = ByteString.EMPTY
)

object BlockHeaderWrapper {

  def apply(blockHeader: BlockHeader): BlockHeaderWrapper =
    BlockHeaderWrapper(
      blockId = BlockId.of(ByteString.copyFrom(getBlockId(blockHeader))),
      blockHeader.parentHeaderId,
      blockHeader.parentSlot,
      blockHeader.txRoot,
      blockHeader.bloomFilter,
      blockHeader.timestamp,
      blockHeader.height,
      blockHeader.slot,
      blockHeader.eligibilityCertificate,
      blockHeader.operationalCertificate,
      blockHeader.metadata,
      blockHeader.address
    )
}
