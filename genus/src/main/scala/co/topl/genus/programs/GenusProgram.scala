package co.topl.genus.programs

import akka.actor.ActorSystem
import akka.grpc.scaladsl.{ServerReflection, ServiceHandler}
import akka.http.scaladsl.Http
import cats.effect.kernel.Async
import cats.implicits._
import cats.{~>, Monad}
import co.topl.genus.algebras._
import co.topl.genus.interpreters.requesthandlers._
import co.topl.genus.services.blocks_query.{BlocksQuery, BlocksQueryHandler}
import co.topl.genus.services.blocks_subscription.{BlocksSubscription, BlocksSubscriptionHandler}
import co.topl.genus.services.transactions_query.{TransactionsQuery, TransactionsQueryHandler}
import co.topl.genus.services.transactions_subscription.{TransactionsSubscription, TransactionsSubscriptionHandler}

import scala.concurrent.Future

object GenusProgram {

  def make[F[_]: Async: Monad: *[_] ~> Future](
    txQuery:         TransactionsQueryService[F],
    txSub:           TransactionsSubscriptionService[F],
    blocksQuery:     BlocksQueryService[F],
    blocksSub:       BlocksSubscriptionService[F],
    ip:              String,
    port:            Int
  )(implicit system: ActorSystem): F[Unit] =
    for {
      handlers <-
        ServiceHandler
          .concatOrNotFound(
            TransactionsQueryHandler.partial(TransactionsQueryImpl.make(txQuery)),
            TransactionsSubscriptionHandler.partial(TransactionsSubscriptionImpl.make(txSub)),
            BlocksQueryHandler.partial(BlocksQueryImpl.make(blocksQuery)),
            BlocksSubscriptionHandler.partial(BlocksSubscriptionImpl.make(blocksSub)),
            ServerReflection.partial(
              List(TransactionsQuery, BlocksQuery, TransactionsSubscription, BlocksSubscription)
            )
          )
          .pure[F]
      binding <-
        Async[F].fromFuture(
          Async[F].delay(
            Http(system).newServerAt(ip, port).bind(handlers)
          )
        )
      _ <- Async[F].delay(println(s"Genus server running: $binding"))
      // run indefinitely
      _ <- Async[F].never[Unit]
    } yield ()
}
