package co.topl.algebras

import co.topl.models.TypedIdentifier

/**
 * Emits a stream of canonical head traversal steps. As blocks are adopted by the node, the steps taken to reach it from
 * the previous adoption are emitted in this stream.
 */
trait CanonicalHeadTraversal[F[_], Stream] {
  def headChanges: F[Stream]
}

/**
 * A forward or backward step along the head of the chain
 */
sealed abstract class CanonicalHeadTraversalStep extends Product with Serializable {
  def blockId: TypedIdentifier
}

object CanonicalHeadTraversalSteps {

  /**
   * A block was appended to the chain
   *
   * @param blockId The block ID that was appended.
   */
  case class Applied(blockId: TypedIdentifier) extends CanonicalHeadTraversalStep

  /**
   * A block was popped from the chain
   *
   * @param blockId The block ID that was rolled back.
   */
  case class Unapplied(blockId: TypedIdentifier) extends CanonicalHeadTraversalStep
}
