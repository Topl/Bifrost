package co.topl.algebras

import co.topl.models.{BlockHeaderV2, TypedIdentifier}

trait BlockHeaderLookup[F[_]] {
  def getBlockHeader(id: TypedIdentifier): F[Option[BlockHeaderV2]]
}
