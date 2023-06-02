package co.topl.minting.algebras

import co.topl.catsutils.Iterative
import co.topl.consensus.models.BlockId
import co.topl.node.models.FullBlockBody

/**
 * Assembles an ideal Block Body using the given parent Block ID.
 */
trait BlockPackerAlgebra[F[_]] {

  /**
   * Constructs an `Iterative` which improves a given Block Body
   */
  def improvePackedBlock(parentBlockId: BlockId, height: Long, slot: Long): F[Iterative[F, FullBlockBody]]
}
