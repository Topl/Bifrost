package co.topl.minting.algebras

import co.topl.consensus.models.BlockId
import co.topl.node.models.FullBlockBody

/**
 * Assembles block bodies
 */
trait BlockPackerAlgebra[F[_]] {

  /**
   * Returns a Stream of incrementally-improving FullBlockBodies using the given parent block information
   */
  def blockImprover(parentBlockId: BlockId, height: Long, slot: Long): fs2.Stream[F, FullBlockBody]
}
