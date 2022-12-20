package co.topl.genusLibrary.model

import co.topl.models.{BlockBodyV2, BlockHeaderV2}

case class HeightData(
  height:    Long,
  blockData: Option[BlockData]
)

case class BlockData(
  header:       BlockHeaderV2,
  body:         BlockBodyV2,
  transactions: BlockBodyV2.Full
)
