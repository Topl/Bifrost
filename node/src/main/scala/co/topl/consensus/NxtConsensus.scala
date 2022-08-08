package co.topl.consensus

import co.topl.modifier.block.Block
import co.topl.utils.Int128

object NxtConsensus {
  case class State(totalStake: Int128, inflation: Long)

  case class Genesis(block: Block, state: NxtConsensus.State)
}
