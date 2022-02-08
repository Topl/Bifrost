package co.topl.genus.algebras

import co.topl.genus.types.BlockHeight

/**
 * Creates a subscription to a data store's data log starting from a chosen offset.
 * @tparam F the effect-ful type of the final value
 * @tparam G the type of collection which data is returned in
 * @tparam Filter the type of filtering that can be used
 * @tparam T the type of data returned
 */
trait DataStoreSubscriptionAlg[F[_], G[_], Filter, T] {
  def fromStart(filter:      Filter): F[G[T]]
  def fromCheckpoint(filter: Filter, checkpoint: BlockHeight): F[G[T]]
}
