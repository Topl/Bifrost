package co.topl.genusLibrary.model

import co.topl.models.{BlockBodyV2, BlockHeaderV2}

case class BlockData(
  header:       BlockHeaderV2,
  body:         BlockBodyV2,
  transactions: BlockBodyV2.Full
)
