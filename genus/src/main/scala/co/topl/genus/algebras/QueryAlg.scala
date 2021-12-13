package co.topl.genus.algebras

trait QueryAlg[F[_], G[_], Filter, T] {
  def query(filter: Filter): F[G[T]]
}
