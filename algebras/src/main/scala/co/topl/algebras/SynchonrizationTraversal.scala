package co.topl.algebras

import co.topl.consensus.models.BlockId

/**
 * Emits a stream of canonical head traversal steps. As blocks are adopted by the node, the steps taken to reach it from
 * the previous adoption are emitted in this stream.
 */
trait SynchronizationTraversal[F[_], G[_]] {
  def headChanges: F[G[SynchronizationTraversalStep]]
}

/**
 * A forward or backward step along the head of the chain
 */
sealed abstract class SynchronizationTraversalStep extends Product with Serializable {
  def blockId: BlockId
}

object SynchronizationTraversalSteps {

  /**
   * A block was appended to the chain
   *
   * @param blockId The block ID that was appended.
   */
  case class Applied(blockId: BlockId) extends SynchronizationTraversalStep

  /**
   * A block was popped from the chain
   *
   * @param blockId The block ID that was rolled back.
   */
  case class Unapplied(blockId: BlockId) extends SynchronizationTraversalStep
}
