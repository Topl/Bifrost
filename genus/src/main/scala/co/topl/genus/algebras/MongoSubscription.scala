package co.topl.genus.algebras

import akka.NotUsed
import akka.stream.scaladsl.Source
import co.topl.genus.typeclasses.MongoFilter
import org.mongodb.scala.Document

/**
 * Represents a subscribe-able Mongo collection of [[Document]] values.
 * @tparam F the effect-ful type of the result
 */
trait MongoSubscription[F[_]] {

  /**
   * Creates a subscription to a Mongo collection with filtered results.
   * The subscription will continuously poll for new values in the database.
   * @param filter the filter for results
   * @tparam Filter the type of filter with an instance of [[MongoFilter]]
   * @return a [[Source]] of [[Document]] values
   */
  def create[Filter: MongoFilter](filter: Filter): F[Source[Document, NotUsed]]
}
