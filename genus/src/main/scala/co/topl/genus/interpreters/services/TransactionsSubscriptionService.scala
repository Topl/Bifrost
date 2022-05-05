package co.topl.genus.interpreters.services

import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.Functor
import cats.data.EitherT
import cats.implicits._
import co.topl.genus.algebras.{MongoSubscription, SubscriptionService}
import co.topl.genus.typeclasses.MongoFilter
import co.topl.genus.typeclasses.implicits._
import co.topl.genus.types.Transaction
import co.topl.utils.mongodb.DocumentDecoder
import co.topl.utils.mongodb.codecs._
import co.topl.utils.mongodb.models.ConfirmedTransactionDataModel

object TransactionsSubscriptionService {

  def make[F[_]: Functor](subscriptions: MongoSubscription[F]): SubscriptionService[F, Transaction] =
    new SubscriptionService[F, Transaction] {

      override def create[Filter: MongoFilter](
        request: SubscriptionService.CreateRequest[Filter]
      ): EitherT[F, SubscriptionService.CreateSubscriptionFailure, Source[Transaction, NotUsed]] =
        EitherT.right[SubscriptionService.CreateSubscriptionFailure](
          subscriptions
            .create(request.filter)
            .map(_.mapConcat(document => DocumentDecoder[ConfirmedTransactionDataModel].fromDocument(document).toSeq))
            .map(_.map(_.transformTo[Transaction]))
        )
    }
}
