package co.topl.genus.interpreters

import cats.implicits._
import akka.actor.ActorSystem
import akka.grpc.scaladsl.{ServerReflection, ServiceHandler}
import akka.http.scaladsl.model.{HttpRequest, HttpResponse}
import akka.http.scaladsl.{Http, ServerBuilder}
import cats.effect.Async
import co.topl.genus.algebras.HttpServer
import co.topl.genus.services.block_query.{BlockQuery, BlockQueryHandler}
import co.topl.genus.services.block_subscription.{BlockSubscription, BlockSubscriptionHandler}
import co.topl.genus.services.transaction_query.{TransactionQuery, TransactionQueryHandler}
import co.topl.genus.services.transaction_subscription.{TransactionSubscription, TransactionSubscriptionHandler}

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

object GenusServer {

  object Mock {

    def make[F[_]: Async](ip: String, port: Int)(implicit
      system:                 ActorSystem
    ): HttpServer[F] =
      new HttpServer[F] {

        val txQueryHandler: PartialFunction[HttpRequest, Future[HttpResponse]] =
          TransactionQueryHandler.partial(TransactionQueryService.Mock.make)

        val blockQueryHandler: PartialFunction[HttpRequest, Future[HttpResponse]] =
          BlockQueryHandler.partial(BlockQueryService.Mock.make)

        val blockSubscriptionServiceHandler: PartialFunction[HttpRequest, Future[HttpResponse]] =
          BlockSubscriptionHandler.partial(BlockSubscriptionService.Mock.make)

        val txSubscriptionServiceHandler: PartialFunction[HttpRequest, Future[HttpResponse]] =
          TransactionSubscriptionHandler.partial(TransactionSubscriptionsService.Mock.make)

        val reflectionServiceHandler: PartialFunction[HttpRequest, Future[HttpResponse]] =
          ServerReflection.partial(
            List(
              TransactionQuery,
              BlockQuery,
              TransactionSubscription,
              BlockSubscription
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
