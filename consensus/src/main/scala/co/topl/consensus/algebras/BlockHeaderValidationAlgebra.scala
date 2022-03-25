package co.topl.consensus.algebras

import co.topl.models.BlockHeaderV2
import co.topl.consensus.BlockHeaderValidationFailure

trait BlockHeaderValidationAlgebra[F[_]] {

  /**
   * Indicates if the claimed child is a valid descendent of the parent
   */
  def validate(child: BlockHeaderV2, parent: BlockHeaderV2): F[Either[BlockHeaderValidationFailure, BlockHeaderV2]]
}
