package co.topl.genus.algebras

trait SubscriptionAlg[F[_], G[_], Filter, Token, T] {
  def subscribe(filter: Filter, lastSeenMessage: Option[Token]): F[G[T]]
}
