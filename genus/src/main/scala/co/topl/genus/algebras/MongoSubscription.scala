package co.topl.genus.algebras

import akka.NotUsed
import akka.stream.scaladsl.Source
import co.topl.genus.typeclasses.{MongoFilter, MongoSort}
import org.mongodb.scala.Document

/**
 * Represents a subscribe-able Mongo collection of [[Document]] values.
 * @tparam F the effect-ful type of the result
 */
trait MongoSubscription[F[_]] {

  /**
   * Creates a subscription to a Mongo collection with filtered results in sorted order.
   * The subscription will continuously poll for new values in the database.
   * @param filter the filter for results
   * @param sort the sorting options for ordering the published values
   * @tparam Filter the type of filter with an instance of [[MongoFilter]]
   * @tparam Sort the type of sorting options with an instance of [[MongoSort]]
   * @return a [[Source]] of [[Document]] values wrapped in an effect-ful context
   */
  def create[Filter: MongoFilter, Sort: MongoSort](
    filter:            Filter,
    sort:              Sort,
    confirmationDepth: Int
  ): F[Source[Document, NotUsed]]
}
