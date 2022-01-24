package co.topl.genus.algebras

import co.topl.genus.services.services_types.Paging

trait DataStoreQueryAlg[F[_], G[_], Sort, Filter, T] {
  def query(filter: Filter, sort: Sort, paging: Option[Paging]): F[G[T]]
}
