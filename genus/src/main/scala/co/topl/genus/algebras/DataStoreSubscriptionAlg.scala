package co.topl.genus.algebras

trait DataStoreSubscriptionAlg[F[_], G[_], Filter, Token, T] {
  def subscribe(filter: Filter, lastSeenMessage: Option[Token]): F[G[T]]
}
