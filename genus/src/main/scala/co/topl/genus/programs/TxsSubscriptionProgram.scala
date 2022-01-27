package co.topl.genus.programs

import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.effect.Async
import cats.~>
import co.topl.genus.algebras.SubscriptionServiceAlg
import co.topl.genus.filters.TransactionFilter
import co.topl.genus.services.transactions_subscription.{
  CreateTxsSubscriptionReq,
  TransactionsSubscription,
  TxsSubscriptionRes
}
import co.topl.genus.types.Transaction
import co.topl.genus.ops.implicits._
import co.topl.genus.typeclasses.implicits._

import scala.concurrent.Future

object TxsSubscriptionProgram {

  object Eval {

    def make[F[_]: Async: *[_] ~> Future](
      subsService: SubscriptionServiceAlg[F, Transaction, TransactionFilter, Long]
    ): TransactionsSubscription =
      (in: CreateTxsSubscriptionReq) =>
        Source
          .futureSource(
            subsService
              .create(in.toRequest)
              .fold(
                failure => Source.single(TxsSubscriptionRes.fromCreateFailure(failure)),
                txs => TxsSubscriptionRes.fromTransactions[Source[*, NotUsed]](txs)
              )
              .mapFunctor
          )
          .mapMaterializedValue(_ => NotUsed)
  }
}
