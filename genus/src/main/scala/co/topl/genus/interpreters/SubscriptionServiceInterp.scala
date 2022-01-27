package co.topl.genus.interpreters

import cats.implicits._
import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.MonadThrow
import cats.data.EitherT
import co.topl.genus.algebras.SubscriptionServiceAlg.{CreateSubscriptionFailure, CreateSubscriptionFailures}
import co.topl.genus.algebras.{DataStoreSubscriptionAlg, SubscriptionServiceAlg}

object SubscriptionServiceInterp {

  object Eval {

    type BlockHeight = Long

    def make[F[_]: MonadThrow, T, Filter](
      defaultFilter: Filter,
      dataStore:     DataStoreSubscriptionAlg[F, Source[*, NotUsed], Filter, BlockHeight, T]
    ): SubscriptionServiceAlg[F, T, Filter, BlockHeight] =
      request =>
        EitherT(
          dataStore
            .subscribe(request.filter.getOrElse(defaultFilter), request.resumeToken)
            .map(_.asRight[CreateSubscriptionFailure])
            .handleError(err => CreateSubscriptionFailures.DataConnectionFailure(err.getMessage).asLeft)
        )
  }
}
