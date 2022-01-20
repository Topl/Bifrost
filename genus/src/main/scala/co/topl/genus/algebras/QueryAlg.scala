package co.topl.genus.algebras

import co.topl.genus.services.services_types.Paging

trait QueryAlg[F[_], G[_], Sorting, Filter, T] {
  def query(filter: Filter, sort: Sorting, paging: Option[Paging]): F[G[T]]
}
