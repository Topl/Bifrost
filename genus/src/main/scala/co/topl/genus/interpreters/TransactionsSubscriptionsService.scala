package co.topl.genus.interpreters

import cats.implicits._
import akka.NotUsed
import akka.stream.scaladsl.Source
import co.topl.genus.services.transactions_subscription.ReadTxsSubscriptionReq.RequestType
import co.topl.genus.services.transactions_subscription._
import co.topl.genus.types.Transaction

import java.util.UUID
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

object TransactionsSubscriptionsService {

  object Mock {

    def make: TransactionsSubscription =
      new TransactionsSubscription {

        override def create(in: CreateTxsSubscriptionReq): Future[CreateTxsSubscriptionRes] =
          Future.successful(
            CreateTxsSubscriptionRes.of(UUID.randomUUID().toString)
          )

        override def delete(in: DeleteTxsSubscriptionReq): Future[DeleteTxsSubscriptionRes] =
          Future.successful(
            DeleteTxsSubscriptionRes.of()
          )

        override def read(in: Source[ReadTxsSubscriptionReq, NotUsed]): Source[ReadTxsSubscriptionRes, NotUsed] =
          in.flatMapConcat(request =>
            request.requestType match {
              case RequestType.Checkpoint(_) => Source.empty
              case RequestType.Pause(_)      => Source.empty
              case RequestType.Start(_) =>
                Source.tick(
                  3.seconds,
                  5.seconds,
                  ReadTxsSubscriptionRes.of(Transaction().withTxId("mock-tx-id").some)
                )
              case RequestType.Empty => Source.empty
            }
          )
      }
  }
}
