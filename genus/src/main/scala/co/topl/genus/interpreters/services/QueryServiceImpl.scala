package co.topl.genus.interpreters.services

import akka.NotUsed
import akka.stream.Materializer
import akka.stream.scaladsl.Source
import cats.MonadThrow
import cats.data.EitherT
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.genus.algebras.QueryService.{QueryFailure, QueryFailures, QueryRequest}
import co.topl.genus.algebras.{MongoQuery, QueryService}
import co.topl.genus.ops.implicits._
import co.topl.genus.typeclasses.{MongoFilter, MongoSort}
import co.topl.genus.typeclasses.implicits._

import scala.concurrent.duration.FiniteDuration

object QueryServiceImpl {

  def make[F[_]: MonadThrow: Async, T](
    dataStore:             MongoQuery[F, T],
    timeout:               FiniteDuration
  )(implicit materializer: Materializer): QueryService[F, T] =
    new QueryService[F, T] {

      override def asList[Filter: MongoFilter, Sort: MongoSort](
        req: QueryRequest[Filter, Sort]
      ): EitherT[F, QueryFailure, List[T]] =
        for {
          source <- asSource(req)
          collectedList <-
            EitherT(
              source
                .collectWithTimeout(timeout)
                .map(_.asRight[QueryFailure])
                .handleError(err => QueryFailures.QueryTimeout(err.getMessage).asLeft)
            ).map(_.toList)
        } yield collectedList

      override def asSource[Filter: MongoFilter, Sort: MongoSort](
        req: QueryRequest[Filter, Sort]
      ): EitherT[F, QueryFailure, Source[T, NotUsed]] =
        for {
          // validate query and convert into a possible `InvalidQuery` failure
          _ <-
            EitherT.fromEither[F](
              req.validate.toEither
                .leftMap(errs => QueryFailures.InvalidQuery(errs))
            )
          // query values as a source
          result <-
            EitherT(
              dataStore
                .query(
                  // use provided filter or default to a filter which gets all values
                  req.filter,
                  req.sort,
                  req.paging
                )
                .map(_.asRight[QueryFailure])
                // handle a Mongo failure as a data store connection error
                .handleError(error => QueryFailures.DataStoreConnectionError(error.getMessage).asLeft)
            )
        } yield result
    }
}
