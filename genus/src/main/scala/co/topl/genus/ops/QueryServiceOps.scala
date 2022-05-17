package co.topl.genus.ops

import akka.stream.Materializer
import akka.stream.scaladsl.Sink
import cats.data.EitherT
import cats.effect.Async
import co.topl.genus.algebras.QueryService
import co.topl.genus.algebras.QueryService.{QueryFailure, QueryRequest}
import co.topl.genus.typeclasses.{MongoFilter, MongoSort}

import scala.language.implicitConversions

class QueryServiceOps[F[_], T](private val value: QueryService[F, T]) extends AnyVal {

  def queryAsList[Filter: MongoFilter, Sort: MongoSort](
    request:         QueryRequest[Filter, Sort]
  )(implicit asyncF: Async[F], materializer: Materializer): EitherT[F, QueryFailure, List[T]] =
    for {
      querySource <- value.query(request)
      resultList  <- EitherT.right[QueryFailure](asyncF.fromFuture(asyncF.delay(querySource.runWith(Sink.seq))))
    } yield resultList.toList
}

object QueryServiceOps {

  trait ToOps {

    implicit def queryServiceOpsFromValue[F[_]: Async, T](value: QueryService[F, T]): QueryServiceOps[F, T] =
      new QueryServiceOps[F, T](value)
  }

  object ops extends ToOps
}
