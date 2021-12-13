package co.topl.genus.algebras

trait SubscriptionAlg[F[_], G[_], Filter, T] {
  def subscribe(filter: Filter): F[G[T]]
}
