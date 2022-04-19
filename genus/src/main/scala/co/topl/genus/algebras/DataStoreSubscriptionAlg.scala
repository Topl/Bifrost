package co.topl.genus.algebras

import cats.implicits._
import cats.Functor
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

object DataStoreSubscriptionAlg {

  /**
   * Maps the resulting value type of the subscription algebra to another type using a provided function.
   * @param algebra the algebra to map from
   * @param fA the function for mapping the underlying value A
   * @tparam F the effectful type of the algebra
   * @tparam G the collection type that the query returns
   * @tparam Filter the filtering type of the algebra
   * @tparam A the underlying type of the algebra to map from
   * @tparam B the underlying type of the new algebra to be created
   * @return a new algebra instance which returns values of type B
   */
  def mapSubscriptionType[F[_]: Functor, G[_]: Functor, Filter, A, B](
    algebra: DataStoreSubscriptionAlg[F, G, Filter, A],
    fA:      A => B
  ): DataStoreSubscriptionAlg[F, G, Filter, B] =
    new DataStoreSubscriptionAlg[F, G, Filter, B] {
      override def fromStart(filter: Filter): F[G[B]] = algebra.fromStart(filter).map(_.map(fA))

      override def fromCheckpoint(filter: Filter, checkpoint: BlockHeight): F[G[B]] =
        algebra.fromCheckpoint(filter, checkpoint).map(_.map(fA))
    }
}
