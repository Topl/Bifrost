package co.topl.genus.algebras

import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.implicits._
import cats.Functor
import co.topl.genus.typeclasses.MongoFilter
import co.topl.genus.types.BlockHeight

/**
 * Creates a subscription to a data store's data log starting from a chosen offset.
 * @tparam F the effect-ful type of the final value
 * @tparam G the type of collection which data is returned in
 * @tparam Filter the type of filtering that can be used
 * @tparam T the type of data returned
 */
trait MongoSubscription[F[_], T] {
  def fromBlockHeight[Filter: MongoFilter](filter: Filter, blockHeight: BlockHeight): F[Source[T, NotUsed]]
}

object MongoSubscription {

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
  def map[F[_]: Functor, A, B](
    algebra: MongoSubscription[F, A],
    fA:      A => B
  ): MongoSubscription[F, B] =
    new MongoSubscription[F, B] {

      override def fromBlockHeight[Filter: MongoFilter](
        filter:     Filter,
        checkpoint: BlockHeight
      ): F[Source[B, NotUsed]] =
        algebra.fromBlockHeight(filter, checkpoint).map(_.map(fA))
    }
}
