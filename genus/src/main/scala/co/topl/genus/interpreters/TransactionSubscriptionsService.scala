package co.topl.genus.interpreters

import cats.implicits._
import akka.NotUsed
import akka.stream.scaladsl.Source
import co.topl.genus.services.transaction_subscription.ReadTxSubscriptionReq.RequestType
import co.topl.genus.services.transaction_subscription._
import co.topl.genus.types.Transaction

import java.util.UUID
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

object TransactionSubscriptionsService {

  object Mock {

    def make: TransactionSubscription =
      new TransactionSubscription {

        override def create(in: CreateTxSubscriptionReq): Future[CreateTxSubscriptionRes] =
          Future.successful(
            CreateTxSubscriptionRes.of(UUID.randomUUID().toString)
          )

        override def delete(in: DeleteTxSubscriptionReq): Future[DeleteTxSubscriptionRes] =
          Future.successful(
            DeleteTxSubscriptionRes.of()
          )

        override def read(in: Source[ReadTxSubscriptionReq, NotUsed]): Source[ReadTxSubscriptionRes, NotUsed] =
          in.flatMapConcat(request =>
            request.requestType match {
              case RequestType.Checkpoint(_) => Source.empty
              case RequestType.Pause(_)      => Source.empty
              case RequestType.Start(_) =>
                Source.tick(
                  3.seconds,
                  5.seconds,
                  ReadTxSubscriptionRes.of(Transaction().withTxId("mock-tx-id").some)
                )
              case RequestType.Empty => Source.empty
            }
          )
      }
  }
}
