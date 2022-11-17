package co.topl.consensus.algebras

import co.topl.consensus.BlockHeaderToBodyValidationFailure
import co.topl.models.BlockV2

trait BlockHeaderToBodyValidationAlgebra[F[_]] {
  def validate(block: BlockV2): F[Either[BlockHeaderToBodyValidationFailure, BlockV2]]
}
