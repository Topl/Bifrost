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

    /**
     * Creates an interpreter of the [[SubscriptionServiceAlg]] with a given default filter value and a
     * set of operations for subscribing to data from a data store.
     * @param defaultFilter the default filter to provide when creating subscriptions when the client has not provided
     *                      any
     * @param dataStore a set of operations for subscribing to data from some underlying data store
     * @tparam F the effect-ful type of the final value in the program
     * @tparam T the type of data that can be subscribed to
     * @tparam Filter the type of filter that can be provided
     * @return a new instance of [[SubscriptionServiceAlg]]
     */
    def make[F[_]: MonadThrow, T, Filter](
      defaultFilter: Filter,
      dataStore:     DataStoreSubscriptionAlg[F, Source[*, NotUsed], Filter, T]
    ): SubscriptionServiceAlg[F, T, Filter] =
      request =>
        for {
          // validate the create request
          _ <- EitherT
            .fromEither[F](request.validate.toEither)
            .leftMap(CreateSubscriptionFailures.InvalidRequest)
          result <-
            EitherT(
              request.startFromHeight
                // if the starting height is provided, then start from that height offset,
                // otherwise, start from the beginning
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
