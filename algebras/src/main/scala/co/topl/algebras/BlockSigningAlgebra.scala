package co.topl.algebras

import co.topl.models.{BlockHeaderV2, Timestamp}

trait BlockSigningAlgebra[F[_]] {
  def sign(unsignedBlockF: Timestamp => BlockHeaderV2.Unsigned): F[BlockHeaderV2]
}
