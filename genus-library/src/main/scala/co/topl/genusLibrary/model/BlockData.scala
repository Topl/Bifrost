package co.topl.genusLibrary.model

import co.topl.models.{BlockBody, BlockHeader}

case class HeightData(
  height:    Long,
  blockData: Option[BlockData]
)

case class BlockData(
  header:       BlockHeader,
  body:         BlockBody,
  transactions: BlockBody.Full
)
