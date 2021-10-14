package co.topl.algebras

import co.topl.models.{BlockBodyV2, BlockHeaderV2, TypedIdentifier}

trait BlockBodyStore[F[_]] {
  def get(id:            TypedIdentifier): F[Option[BlockBodyV2]]
  def put(blockHeaderV2: BlockHeaderV2): F[Unit]
  def remove(id:         TypedIdentifier): F[Option[BlockBodyV2]]
}
