package co.topl.genus.interpreters.services

import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.data.EitherT
import cats.{~>, MonadThrow}
import co.topl.genus.algebras.{MongoSubscription, SubscriptionService}
import co.topl.genus.services.transactions_query.TransactionSorting
import co.topl.genus.typeclasses.implicits._
import co.topl.genus.typeclasses.{MongoFilter, WithMaxBlockHeight}
import co.topl.genus.types.Transaction

import scala.concurrent.Future

object TransactionsSubscriptionService {

  def make[F[_]: MonadThrow: *[_] ~> Future](
    subscriptions: MongoSubscription[F]
  ): SubscriptionService[F, Transaction] =
    new SubscriptionService[F, Transaction] {

      override def create[Filter: MongoFilter: WithMaxBlockHeight](
        request: SubscriptionService.CreateRequest[Filter]
      ): EitherT[F, SubscriptionService.CreateSubscriptionFailure, Source[Transaction, NotUsed]] =
        MonadThrow[F]
          // catch a possible failure with creating the subscription
          .attemptT(
            subscriptions
              .create(
                request.filter,
                TransactionSorting(TransactionSorting.SortBy.Height(TransactionSorting.Height())),
                request.confirmationDepth
              )
          )
          .leftMap[SubscriptionService.CreateSubscriptionFailure](failure =>
            SubscriptionService.CreateSubscriptionFailures.DataConnectionFailure(failure.getMessage)
          )
          .map(source => source.mapConcat(documentToTransaction(_).toSeq))
    }
}
