package co.topl.consensus.algebras

import co.topl.consensus.BlockHeaderToBodyValidationFailure
import co.topl.models.Block

trait BlockHeaderToBodyValidationAlgebra[F[_]] {
  def validate(block: Block): F[Either[BlockHeaderToBodyValidationFailure, Block]]
}
