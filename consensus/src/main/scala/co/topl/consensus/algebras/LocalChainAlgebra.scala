package co.topl.consensus.algebras

import co.topl.consensus.models.{BlockId, SlotData}

/**
 * Operations involving this node's locally-adopted canonical chain
 */
trait LocalChainAlgebra[F[_]] {

  /**
   * Instructs the node to adopt the given canonical head.  This head, along with all of its ancestors, should be
   * pre-validated elsewhere.
   *
   * @param newHead The new _valid_ canonical head slot to adopt
   */
  def adopt(newHead: SlotData): F[Unit]

  /**
   * Emits a stream of _new_ BlockIds that have been adopted by this node, excluding the current head
   */
  def adoptions: F[fs2.Stream[F, BlockId]]

  /**
   * The head of the chain that has been adopted locally by this node.
   */
  def head: F[SlotData]

  /**
   * The first block (SlotData) in the chain
   */
  def genesis: F[SlotData]
  def chainSelection: F[ChainSelectionAlgebra[F]]

  def blockIdAtHeight(height: Long): F[Option[BlockId]]
}
