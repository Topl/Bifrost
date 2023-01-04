package co.topl.consensus.algebras

import co.topl.models.BlockHeader
import co.topl.consensus.BlockHeaderValidationFailure

trait BlockHeaderValidationAlgebra[F[_]] {

  /**
   * Indicates if the claimed child is a valid descendent of the parent
   */
  def validate(child: BlockHeader, parent: BlockHeader): F[Either[BlockHeaderValidationFailure, BlockHeader]]
}
