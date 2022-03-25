package co.topl.genus.algebras

import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.Functor
import cats.implicits._
import co.topl.genus.services.services_types.Paging
import co.topl.genus.typeclasses.{MongoFilter, MongoSort}

/**
 * Represents a data store that can be queried with some additional options.
 * @tparam F the effect-ful type of the final value
 * @tparam G the collection type of the resulting values
 * @tparam Sort a type which can be used to sort results
 * @tparam Filter a type which can be used to filter results
 * @tparam T the type of values stored in this data set
 */
trait MongoQuery[F[_], T] {

  def query[Filter: MongoFilter, Sort: MongoSort](
    filter: Filter,
    sort:   Sort,
    paging: Option[Paging]
  ): F[Source[T, NotUsed]]
}

object MongoQuery {

  /**
   * Maps the resulting value type of the query algebra to another type using a provided function.
   * @param dataStoreQuery the algebra to map from
   * @param fA the function for mapping the underlying value A
   * @tparam F the effect-ful type of the algebra
   * @tparam G the collection type that the query returns
   * @tparam Sort the sorting type of the algebra
   * @tparam Filter the filtering type of the algebra
   * @tparam A the underlying type of the algebra to map from
   * @tparam B the underlying type of the new algebra to be created
   * @return a new algebra instance which returns values of type B
   */
  def map[F[_]: Functor, A, B](
    dataStoreQuery: MongoQuery[F, A],
    fA:             A => B
  ): MongoQuery[F, B] =
    new MongoQuery[F, B] {

      override def query[Filter: MongoFilter, Sort: MongoSort](
        filter: Filter,
        sort:   Sort,
        paging: Option[Paging]
      ): F[Source[B, NotUsed]] = dataStoreQuery.query(filter, sort, paging).map(_.map(fA))
    }
}
