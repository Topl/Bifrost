package co.topl.genus.interpreters.services

import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.MonadThrow
import cats.data.EitherT
import co.topl.genus.algebras.SubscriptionService.{CreateSubscriptionFailure, CreateSubscriptionFailures}
import co.topl.genus.algebras.{MongoSubscription, SubscriptionService}
import co.topl.genus.typeclasses.implicits._
import co.topl.genus.typeclasses.{MongoFilter, Transform}
import co.topl.genus.types.BlockHeight

object MongoSubscriptionService {

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
  def make[F[_]: MonadThrow, Data, Model](
    dataStore:          MongoSubscription[F, Data]
  )(implicit transform: Transform[Data, Model]): SubscriptionService[F, Model] =
    new SubscriptionService[F, Model] {

      override def create[Filter: MongoFilter](
        request: SubscriptionService.CreateRequest[Filter]
      ): EitherT[F, CreateSubscriptionFailure, Source[Model, NotUsed]] =
        for {
          // validate the create request
          _ <- EitherT
            .fromEither[F](request.validate.toEither)
            .leftMap(CreateSubscriptionFailures.InvalidRequest)
          data <-
            EitherT.right[SubscriptionService.CreateSubscriptionFailure](
              dataStore
                .fromBlockHeight(
                  request.filter,
                  request.startFromHeight.getOrElse(BlockHeight.defaultInstance)
                )
            )
          models = data.map(_.transformTo[Model])
        } yield models
    }
}
