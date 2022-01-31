package co.topl.genus.interpreters

import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.{Applicative, MonadThrow}
import cats.data.EitherT
import cats.implicits._
import co.topl.genus.algebras.SubscriptionServiceAlg.{CreateSubscriptionFailure, CreateSubscriptionFailures}
import co.topl.genus.algebras.{DataStoreSubscriptionAlg, SubscriptionServiceAlg}
import co.topl.genus.typeclasses.implicits._

object SubscriptionServiceInterp {

  object Eval {

    def make[F[_]: MonadThrow, T, Filter](
      defaultFilter: Filter,
      dataStore:     DataStoreSubscriptionAlg[F, Source[*, NotUsed], Filter, T]
    ): SubscriptionServiceAlg[F, T, Filter] =
      request =>
        for {
          _ <- EitherT
            .fromEither[F](request.validate.toEither)
            .leftMap(CreateSubscriptionFailures.InvalidRequest)
          result <-
            EitherT(
              request.startFromHeight
                .fold(dataStore.fromStart(request.filter.getOrElse(defaultFilter)))(height =>
                  dataStore.fromCheckpoint(request.filter.getOrElse(defaultFilter), height)
                )
                .map(_.asRight[CreateSubscriptionFailure])
                .handleError(err => CreateSubscriptionFailures.DataConnectionFailure(err.getMessage).asLeft)
            )
        } yield result
  }

  object Mock {

    def make[F[_]: Applicative, T, Filter](results: List[T]): SubscriptionServiceAlg[F, T, Filter] =
      _ => EitherT.right(Source.repeat(results).flatMapConcat(list => Source(list)).pure[F])

  }
}
