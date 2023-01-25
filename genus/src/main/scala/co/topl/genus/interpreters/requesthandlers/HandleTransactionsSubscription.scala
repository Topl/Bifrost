package co.topl.genus.interpreters.requesthandlers

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.scaladsl.Source
import cats.effect.Async
import cats.{~>, Monad}
import co.topl.genus.algebras.SubscriptionService
import co.topl.genus.ops.implicits._
import co.topl.genus.services.transactions_subscription._
import co.topl.genus.typeclasses.implicits._
import co.topl.genus.types.Transaction

import scala.concurrent.Future

object HandleTransactionsSubscription {

  def make[F[_]: Async: Monad: *[_] ~> Future](
    subscriptions: SubscriptionService[F, Transaction]
  )(implicit system: ActorSystem): TransactionsSubscription =
    (in: CreateTxsSubscriptionReq) =>
      Source
        .futureSource(
          subscriptions
            .create(in.toRequest)
            .fold(
              failure => Source.single(TxsSubscriptionRes.fromCreateFailure(failure)),
              txs => TxsSubscriptionRes.fromTransactions[Source[*, NotUsed]](txs)
            )
            .mapFunctor
        )
        .mapMaterializedValue(_ => NotUsed)
}
