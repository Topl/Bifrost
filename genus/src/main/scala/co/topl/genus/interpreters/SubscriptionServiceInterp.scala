package co.topl.genus.interpreters

import cats.implicits._
import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.MonadThrow
import cats.data.EitherT
import cats.implicits._
import co.topl.genus.algebras.SubscriptionServiceAlg.{CreateSubscriptionFailure, CreateSubscriptionFailures}
import co.topl.genus.algebras.{DataStoreSubscriptionAlg, SubscriptionServiceAlg}

object SubscriptionServiceInterp {

  object Eval {

    def make[F[_]: MonadThrow, T, Filter](
      defaultFilter: Filter,
      dataStore:     DataStoreSubscriptionAlg[F, Source[*, NotUsed], Filter, Long, T]
    ): SubscriptionServiceAlg[F, T, Filter, Long] =
      request =>
        EitherT(
          request.resumeToken
            .fold(dataStore.fromStart(request.filter.getOrElse(defaultFilter)))(height =>
              dataStore.fromCheckpoint(request.filter.getOrElse(defaultFilter), height)
            )
            .map(_.asRight[CreateSubscriptionFailure])
            .handleError(err => CreateSubscriptionFailures.DataConnectionFailure(err.getMessage).asLeft)
        )
  }
}
