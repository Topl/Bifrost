package co.topl.genus.interpreters.requesthandlers

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.scaladsl.Source
import cats.effect.Async
import cats.implicits._
import cats.{~>, Monad}
import co.topl.genus.algebras.TransactionsSubscriptionService
import co.topl.genus.ops.implicits._
import co.topl.genus.services.transactions_subscription._
import co.topl.genus.typeclasses.implicits._

import scala.concurrent.Future

object TransactionsSubscriptionImpl {

  def make[F[_]: Async: Monad: *[_] ~> Future](
    subscriptionsService: TransactionsSubscriptionService[F]
  )(implicit system:      ActorSystem): TransactionsSubscription =
    (in: CreateTxsSubscriptionReq) =>
      Source
        .futureSource(
          subscriptionsService
            .create(in.toRequest)
            .fold(
              failure => Source.single(TxsSubscriptionRes.fromCreateFailure(failure)),
              txs => TxsSubscriptionRes.fromTransactions[Source[*, NotUsed]](txs)
            )
            .map(_.wireTap(result => println(result)))
            .mapFunctor
        )
        .mapMaterializedValue(_ => NotUsed)
}
