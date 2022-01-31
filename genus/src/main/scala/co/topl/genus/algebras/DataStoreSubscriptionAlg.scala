package co.topl.genus.algebras

import co.topl.genus.types.BlockHeight

trait DataStoreSubscriptionAlg[F[_], G[_], Filter, T] {
  def fromStart(filter:      Filter): F[G[T]]
  def fromCheckpoint(filter: Filter, checkpoint: BlockHeight): F[G[T]]
}
