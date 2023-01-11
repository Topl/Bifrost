package co.topl.blockchain.algebras

import co.topl.blockchain.models.BlockHeaderToBodyValidationFailure
import co.topl.models.Block

trait BlockHeaderToBodyValidationAlgebra[F[_]] {
  def validate(block: Block): F[Either[BlockHeaderToBodyValidationFailure, Block]]
}
