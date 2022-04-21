package co.topl.genus.algebras

import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.data.{EitherT, NonEmptyChain}
import co.topl.genus.services.services_types.Paging
import co.topl.genus.typeclasses.{MongoFilter, MongoSort}

/**
 * Represents a service which can handle query requests for data from some data source.
 * @tparam F the effect-ful type of the final values
 * @tparam T the type of values contained in the queried data set
 * @tparam Filter the type of filters that can be used in a query
 * @tparam Sort the type of sorting that can be used in a query
 */
trait QueryService[F[_], T] {
  import QueryService._

  /**
   * Queries the data source and returns values as a list if successful.
   * @param request the query request containing options for data to be returned
   * @return if successful, a list of values matching the query, otherwise a failure
   */
  def asList[Filter: MongoFilter, Sort: MongoSort](
    request: QueryRequest[Filter, Sort]
  ): EitherT[F, QueryFailure, List[T]]

  /**
   * Queries the data source and returns values as a Source if successful.
   *
   * It is expected that the source will terminate at some point.
   *
   * @param request the query request containing options for data to be returned
   * @return if successful, a list of values matching the query, otherwise a failure
   */
  def asSource[Filter: MongoFilter, Sort: MongoSort](
    request: QueryRequest[Filter, Sort]
  ): EitherT[F, QueryFailure, Source[T, NotUsed]]
}

object QueryService {

  /**
   * A generic query request for some data with optional sorting, filtering, and paging.
   * @param filter a filter which matches particular documents
   * @param sort a value which orders the returned collection
   * @param paging provides information for tabulating the results
   * @param confirmationDepth the minimum distance between the block of the returned results and the current head of
   *                          the chain
   * @tparam Filter the type of filtering value
   * @tparam Sort the type of sorting value
   */
  case class QueryRequest[Filter, Sort](
    filter:            Filter,
    sort:              Sort,
    paging:            Option[Paging],
    confirmationDepth: Int
  )

  /**
   * A failure that occurred while querying for data.
   */
  sealed trait QueryFailure

  object QueryFailures {

    /**
     * The query request contained some invalid options.
     * @param failures the list of reasons why the query was invalid
     */
    case class InvalidQuery(failures: NonEmptyChain[String]) extends QueryFailure

    /**
     * An error occurred while retrieving data from the data store.
     * @param failure the failure reason
     */
    case class DataStoreConnectionError(failure: String) extends QueryFailure

    /**
     * The query took too long to complete.
     * @param message an additional message on why the wquery timed out
     */
    case class QueryTimeout(message: String) extends QueryFailure
  }
}
