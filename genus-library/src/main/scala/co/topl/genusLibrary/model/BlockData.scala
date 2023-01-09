package co.topl.genusLibrary.model

import co.topl.models.{BlockBody, BlockHeader}
import co.topl.consensus.models.{BlockHeader => ConsensusBlockHeader} // TODO remove rename, after remove models

case class HeightData(
  height:    Long,
  blockData: Option[BlockData]
)

case class BlockData(
  header:       ConsensusBlockHeader,
  body:         BlockBody,
  transactions: BlockBody.Full
)
