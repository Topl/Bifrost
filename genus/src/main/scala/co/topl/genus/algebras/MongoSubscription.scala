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
  def create[Filter: MongoFilter](filter: Filter): F[Source[T, NotUsed]]
}
