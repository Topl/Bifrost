package co.topl.algebras

import co.topl.models.{BlockBodyV2, BlockHeaderV2, BlockV2, SlotId}

trait BlockStorageAlgebra[F[_]] {
  def add(block:         BlockV2): F[_]
  def getBlock(id:       SlotId): F[Option[BlockV2]]
  def getBlockHeader(id: SlotId): F[Option[BlockHeaderV2]]
  def getBlockBody(id:   SlotId): F[Option[BlockBodyV2]]
}
