package co.topl.genus.algebras

import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.data.{EitherT, NonEmptyChain}
import co.topl.genus.services.services_types.Paging

trait QueryServiceAlg[F[_], T, Filter, Sort] {
  import QueryServiceAlg._

  def asList(request: QueryRequest[Filter, Sort]): EitherT[F, QueryFailure, List[T]]

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
