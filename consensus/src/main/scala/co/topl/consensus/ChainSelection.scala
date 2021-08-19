package co.topl.consensus

import co.topl.models.BlockHeaderV2

/**
 * Maintains a pool of tines.  Inserting a Block will update the internal tine pool, and a (potentially unchanged) tine
 * is selected and returned.
 */
trait ChainSelection {

  /**
   * Writes the given block to the consensus tine pool.
   * @return (the updated Consensus, the currently selected Tine)
   */
  def withBlock(block: BlockHeaderV2): (ChainSelection, ChainSelection.Tine)

  /**
   * Invalidates a Tine from the tine pool, perhaps when a Ledger indicates semantically invalid blocks.  A new
   * ChainSelection and selected Tine are returned.
   */
  def invalidatedTine(tine: ChainSelection.Tine): (ChainSelection, ChainSelection.Tine)
}

object ChainSelection {
  type Tine = List[BlockHeaderV2]
}
