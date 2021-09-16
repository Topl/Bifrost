package co.topl.algebras

import co.topl.models.BlockHeaderV2

trait BlockHeaderValidationAlgebra[F[_]] {

  /**
   * Indicates if the claimed child is a valid descendent of the parent
   */
  def validate(child: BlockHeaderV2, parent: BlockHeaderV2): F[BlockHeaderV2]
}
