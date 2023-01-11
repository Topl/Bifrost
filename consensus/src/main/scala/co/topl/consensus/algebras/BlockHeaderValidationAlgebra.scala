package co.topl.consensus.algebras

import co.topl.consensus.models.BlockHeaderValidationFailure
import co.topl.consensus.models.{BlockHeader => ConsensusBlockHeader}

trait BlockHeaderValidationAlgebra[F[_]] {

  /**
   * Indicates if the claimed child is a valid descendent of the parent
   */
  def validate(
    child:  ConsensusBlockHeader,
    parent: ConsensusBlockHeader
  ): F[Either[BlockHeaderValidationFailure, ConsensusBlockHeader]]
}
