package co.topl.genusLibrary.model

import cats.data.Chain
import co.topl.node.models.BlockBody
import co.topl.consensus.models.BlockHeader
import co.topl.brambl.models.transaction.IoTransaction

/**
 * Data structure that encapsulates relation between a possible block data and its height.
 * @param height block height
 * @param blockData optional block data
 */
case class HeightData(
  height:    Long,
  blockData: Option[BlockData]
)

/**
 * Data structure with the most important parts of a Block. Equivalent to a denormalized Block.Full.
 * @param header block header
 * @param body block body. TypedIdentifiers of the block transactions.
 * @param transactions block transactions
 */
case class BlockData( // TODO remove
  header:       BlockHeader,
  body:         BlockBody,
  transactions: Chain[IoTransaction]
)
