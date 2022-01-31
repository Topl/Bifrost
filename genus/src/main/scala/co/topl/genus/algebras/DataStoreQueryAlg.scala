package co.topl.genus.algebras

import co.topl.genus.services.services_types.Paging

/**
 * Represents a data store that can be queried with some additional options.
 * @tparam F the effect-ful type of the final value
 * @tparam G the collection type of the resulting values
 * @tparam Sort a type which can be used to sort results
 * @tparam Filter a type which can be used to filter results
 * @tparam T the type of values stored in this data set
 */
trait DataStoreQueryAlg[F[_], G[_], Sort, Filter, T] {
  def query(filter: Filter, sort: Sort, paging: Option[Paging]): F[G[T]]
}
