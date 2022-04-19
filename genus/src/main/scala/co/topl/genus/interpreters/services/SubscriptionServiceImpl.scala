package co.topl.genus.interpreters.services

import cats.implicits._
import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.data.EitherT
import cats.{Applicative, MonadThrow}
import co.topl.genus.algebras.SubscriptionService.{CreateSubscriptionFailure, CreateSubscriptionFailures}
import co.topl.genus.algebras.{MongoSubscription, SubscriptionService}
import co.topl.genus.typeclasses.MongoFilter
import co.topl.genus.typeclasses.implicits._

object SubscriptionServiceImpl {

  /**
   * Creates an interpreter of the [[SubscriptionService]] with a given default filter value and a
   * set of operations for subscribing to data from a data store.
   *
   * @param defaultFilter the default filter to provide when creating subscriptions when the client has not provided
   *                      any
   * @param dataStore a set of operations for subscribing to data from some underlying data store
   * @tparam F the effect-ful type of the final value in the program
   * @tparam T the type of data that can be subscribed to
   * @tparam Filter the type of filter that can be provided
   * @return a new instance of [[SubscriptionService]]
   */
  def make[F[_]: MonadThrow, T](
    dataStore: MongoSubscription[F, T]
  ): SubscriptionService[F, T] =
    new SubscriptionService[F, T] {

      override def create[Filter: MongoFilter](
        request: SubscriptionService.CreateRequest[Filter]
      ): EitherT[F, CreateSubscriptionFailure, Source[T, NotUsed]] =
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
                .fold(dataStore.fromStart(request.filter))(height => dataStore.fromBlockHeight(request.filter, height))
                .map(_.asRight[CreateSubscriptionFailure])
                .handleError(err => CreateSubscriptionFailures.DataConnectionFailure(err.getMessage).asLeft)
            )
        } yield result
    }
}
