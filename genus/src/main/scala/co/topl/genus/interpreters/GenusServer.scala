package co.topl.genus.interpreters

import cats.implicits._
import akka.actor.ActorSystem
import akka.grpc.scaladsl.{ServerReflection, ServiceHandler}
import akka.http.scaladsl.model.{HttpRequest, HttpResponse}
import akka.http.scaladsl.{Http, ServerBuilder}
import cats.effect.Async
import co.topl.genus.algebras.HttpServer
import co.topl.genus.services.blocks_query.{BlocksQuery, BlocksQueryHandler}
import co.topl.genus.services.blocks_subscription.{BlocksSubscription, BlocksSubscriptionHandler}
import co.topl.genus.services.transactions_query.{TransactionsQuery, TransactionsQueryHandler}
import co.topl.genus.services.transactions_subscription.{TransactionsSubscription, TransactionsSubscriptionHandler}

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

object GenusServer {

  object Mock {

    def make[F[_]: Async](ip: String, port: Int)(implicit
      system:                 ActorSystem
    ): HttpServer[F] =
      new HttpServer[F] {

        val txQueryHandler: PartialFunction[HttpRequest, Future[HttpResponse]] =
          TransactionsQueryHandler.partial(TransactionsQueryService.Mock.make)

        val blockQueryHandler: PartialFunction[HttpRequest, Future[HttpResponse]] =
          BlocksQueryHandler.partial(BlocksQueryService.Mock.make)

        val blockSubscriptionServiceHandler: PartialFunction[HttpRequest, Future[HttpResponse]] =
          BlocksSubscriptionHandler.partial(BlockSubscriptionService.Mock.make)

        val txSubscriptionServiceHandler: PartialFunction[HttpRequest, Future[HttpResponse]] =
          TransactionsSubscriptionHandler.partial(TransactionsSubscriptionsService.Mock.make)

        val reflectionServiceHandler: PartialFunction[HttpRequest, Future[HttpResponse]] =
          ServerReflection.partial(
            List(
              TransactionsQuery,
              BlocksQuery,
              TransactionsSubscription,
              BlocksSubscription
            )
          )

        val serviceHandler: HttpRequest => Future[HttpResponse] =
          ServiceHandler.concatOrNotFound(
            txQueryHandler,
            blockQueryHandler,
            txSubscriptionServiceHandler,
            blockSubscriptionServiceHandler,
            reflectionServiceHandler
          )

        val builder: ServerBuilder = Http(system).newServerAt(ip, port)

        override def run: F[Http.ServerBinding] =
          Async[F]
            .fromFuture(
              builder
                .bind(serviceHandler)
                .pure[F]
            )
            .map(_.addToCoordinatedShutdown(hardTerminationDeadline = 10.seconds))
      }
  }
}
