package co.topl.genus.algebras

import akka.NotUsed
import akka.stream.scaladsl.Source
import co.topl.genus.services.services_types.Paging
import co.topl.genus.typeclasses.{MongoFilter, MongoSort}
import org.mongodb.scala.Document

/**
 * Represents a query-able mongo collection of [[Document]] values.
 * @tparam F the effect-ful type of the final value
 */
trait MongoQuery[F[_]] {

  /**
   * Sends a query request to the collection.
   * @param filter the filter to apply on the documents
   * @param sort the sort for ordering documents
   * @param paging the pagin options for returned documents
   * @tparam Filter a type with an instance of [[MongoFilter]]
   * @tparam Sort a type with an instance of [[MongoSort]]
   * @return a [[Source]] of [[Document]] values
   */
  def query[Filter: MongoFilter, Sort: MongoSort](
    filter: Filter,
    sort:   Sort,
    paging: Option[Paging]
  ): F[Source[Document, NotUsed]]
}
