package co.topl.genus.algebras

trait DataStoreSubscriptionAlg[F[_], G[_], Filter, Token, T] {
  def fromStart(filter:      Filter): F[G[T]]
  def fromCheckpoint(filter: Filter, checkpoint: Token): F[G[T]]
}
