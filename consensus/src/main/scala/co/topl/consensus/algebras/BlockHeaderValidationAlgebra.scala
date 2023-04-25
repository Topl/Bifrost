package co.topl.consensus.algebras

import co.topl.consensus.models.BlockHeaderValidationFailure
import co.topl.consensus.models.BlockHeader

trait BlockHeaderValidationAlgebra[F[_]] {

  /**
   * Indicates if the claimed child is a valid descendent of the parent
   */
  def validate(header: BlockHeader): F[Either[BlockHeaderValidationFailure, BlockHeader]]
}
