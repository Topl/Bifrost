package co.topl.fullnode

import cats.data.NonEmptyChain
import co.topl.models.utility.Ratio
import co.topl.models.{BlockV2, Bytes, TaktikosAddress}

/**
 * @param tines
 * @param relativeStake from N-2 Epoch
 * @param epochNonce from N-2 Epoch
 */
case class InMemoryState(tines: NonEmptyChain[Tine], relativeStake: Map[TaktikosAddress, Ratio], epochNonce: Bytes) {

  def append(nextBlock: BlockV2): InMemoryState = copy(
    NonEmptyChain.fromChainPrepend(tines.head.append(nextBlock), tines.tail)
  )
  def head: BlockV2 = tines.head.blocks.last
}

case class Tine(blocks: NonEmptyChain[BlockV2]) {
  def append(nextBlock: BlockV2): Tine = copy(blocks.append(nextBlock))
}
