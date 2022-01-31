package co.topl.genus.algebras

import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.data.{EitherT, NonEmptyChain}
import co.topl.genus.services.services_types.Paging

/**
 * Represents a service which can handle query requests for data from some data source.
 * @tparam F the effect-ful type of the final values
 * @tparam T the type of values contained in the queried data set
 * @tparam Filter the type of filters that can be used in a query
 * @tparam Sort the type of sorting that can be used in a query
 */
trait QueryServiceAlg[F[_], T, Filter, Sort] {
  import QueryServiceAlg._

  /**
   * Queries the data source and returns values as a list if successful.
   * @param request the query request containing options for data to be returned
   * @return if successful, a list of values matching the query, otherwise a failure
   */
  def asList(request: QueryRequest[Filter, Sort]): EitherT[F, QueryFailure, List[T]]

  /**
   * Queries the data source and returns values as a Source if successful.
   *
   * It is expected that the source will terminate at some point.
   *
   * @param request the query request containing options for data to be returned
   * @return if successful, a list of values matching the query, otherwise a failure
   */
  def asSource(request: QueryRequest[Filter, Sort]): EitherT[F, QueryFailure, Source[T, NotUsed]]
}

object QueryServiceAlg {

  case class QueryRequest[Filter, Sort](
    filter:            Option[Filter],
    sort:              Option[Sort],
    paging:            Option[Paging],
    confirmationDepth: Int
  )

  sealed trait QueryFailure

  object QueryFailures {

    case class InvalidQuery[Filter, Sort](failures: NonEmptyChain[String]) extends QueryFailure

    case class DataStoreConnectionError(failure: String) extends QueryFailure

    case class QueryTimeout(message: String) extends QueryFailure
  }
}
