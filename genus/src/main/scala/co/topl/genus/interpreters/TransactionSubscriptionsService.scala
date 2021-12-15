package co.topl.genus.interpreters

import akka.NotUsed
import akka.stream.scaladsl.Source
import co.topl.genus.services.transaction_subscriptions._

import scala.concurrent.Future

object TransactionSubscriptionsService {

  object Mock {

    def make: TransactionSubscriptions =
      new TransactionSubscriptions {
        override def create(in: CreateTxSubscriptionReq): Future[CreateTxSubscriptionRes] = ???

        override def delete(in: DeleteTxSubscriptionReq): Future[DeleteTxSubscriptionRes] = ???

        override def read(in: Source[ReadTxSubscriptionReq, NotUsed]): Source[ReadTxSubscriptionRes, NotUsed] = ???

        override def status(in: TxSubscriptionStatusReq): Future[TxSubscriptionStatusRes] = ???
      }
  }
}
