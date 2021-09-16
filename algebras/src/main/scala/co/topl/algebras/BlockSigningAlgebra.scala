package co.topl.algebras

import co.topl.models.{BlockV2, Timestamp}

trait BlockSigningAlgebra[F[_]] {
  def sign(unsignedBlockF: Timestamp => BlockV2.Unsigned): F[BlockV2]
}
