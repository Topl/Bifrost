package co.topl.consensus.algebras

import cats.data.Validated
import co.topl.consensus.models.{BlockId, SlotData}

/**
 * Operations involving this node's locally-adopted canonical chain
 */
trait LocalChainAlgebra[F[_]] {

  /**
   * Indicates if the provided "newHead" results in a better chain than the current local chain.
   *
   * The `newHead` _can_ be invalid.  (For example, the block needs to have the proper syntax,
   * but it may not necessarily need to be validated for consensus and ledger purposes)
   *
   * @param newHead The head of a new tine, either from a network peer or from a local staker
   * @return True if the provided segment is better than the local canonical chain
   */
  def isWorseThan(newHead: SlotData): F[Boolean]

  /**
   * Instructs the node to adopt the given canonical head.  This head, along with all of its ancestors, should be
   * pre-validated elsewhere.
   *
   * @param newHead The new _valid_ canonical head slot to adopt
   */
  def adopt(newHead: Validated.Valid[SlotData]): F[Unit]

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
}
