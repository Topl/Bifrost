package co.topl.genus.interpreters

import akka.actor.ClassicActorSystemProvider
import akka.grpc.scaladsl.{ServerReflection, ServiceHandler}
import akka.http.scaladsl.Http.HttpServerTerminated
import akka.http.scaladsl.model.{HttpRequest, HttpResponse}
import akka.http.scaladsl.{Http, ServerBuilder}
import cats.Applicative
import cats.effect.Async
import cats.implicits._
import co.topl.genus.algebras.HttpServerAlg
import co.topl.genus.services.blocks_query.{BlocksQuery, BlocksQueryHandler}
import co.topl.genus.services.transactions_query.{TransactionsQuery, TransactionsQueryHandler}

import java.net.InetSocketAddress
import scala.concurrent.Future
import scala.concurrent.duration.{DurationInt, FiniteDuration}

object QueryServer {

  object Eval {

    def make[F[_]: Async](
      txsQuery:    TransactionsQuery,
      blocksQuery: BlocksQuery
    )(ip:          String, port: Int)(implicit system: ClassicActorSystemProvider): HttpServerAlg[F] =
      new HttpServerAlg[F] {

        val txsQueryHandler: PartialFunction[HttpRequest, Future[HttpResponse]] =
          TransactionsQueryHandler.partial(txsQuery)

        val blocksQueryHandler: PartialFunction[HttpRequest, Future[HttpResponse]] =
          BlocksQueryHandler.partial(blocksQuery)

        val reflectionServiceHandler: PartialFunction[HttpRequest, Future[HttpResponse]] =
          ServerReflection.partial(
            List(TransactionsQuery, BlocksQuery)
          )

        val serviceHandler: HttpRequest => Future[HttpResponse] =
          ServiceHandler.concatOrNotFound(
            txsQueryHandler,
            blocksQueryHandler,
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

  object Mock {

    def make[F[_]: Applicative]: HttpServerAlg[F] = new HttpServerAlg[F] {

      override def run: F[Http.ServerBinding] =
        Http
          .ServerBinding(InetSocketAddress.createUnresolved("0.0.0.0", 8080))(
            () => Future.successful(()),
            (_: FiniteDuration) => Future.successful(HttpServerTerminated)
          )
          .pure[F]
    }
  }
}
