package co.topl.genus.programs

import akka.actor.ActorSystem
import akka.event.Logging
import akka.grpc.scaladsl.{ServerReflection, ServiceHandler, WebHandler}
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.{Directive0, Directives, Route}
import akka.http.scaladsl.server.Directives.{handle, optionalHeaderValueByName, pass, reject}
import cats.effect.kernel.Async
import cats.implicits._
import cats.{~>, Monad}
import co.topl.crypto.hash.blake2b256
import co.topl.genus.services.blocks_query.{BlocksQuery, BlocksQueryHandler}
import co.topl.genus.services.blocks_subscription.{BlocksSubscription, BlocksSubscriptionHandler}
import co.topl.genus.services.transactions_query.{TransactionsQuery, TransactionsQueryHandler}
import co.topl.genus.services.transactions_subscription.{TransactionsSubscription, TransactionsSubscriptionHandler}
import co.topl.utils.StringDataTypes.Base58Data
import co.topl.utils.implicits._

import scala.concurrent.Future
import scala.util.{Failure, Success}

object GenusProgram {

  def useAuth(apiKeyHash: Option[Base58Data]): Directive0 =
    optionalHeaderValueByName("x-api-key").flatMap { keyOpt =>
      (keyOpt, apiKeyHash) match {
        case (Some(provided), Some(needed)) =>
          // check that the provided key hashes to the expected value
          val hashedKey = Base58Data.fromData(blake2b256.hash(provided.getBytes("UTF-8")).value)

          if (hashedKey === needed) pass
          else reject

        case (None, Some(_)) => reject
        case _               => pass
      }
    }

  def make[F[_]: Async: Monad: *[_] ~> Future](
    txQueryHandler:     TransactionsQuery,
    txSubHandler:       TransactionsSubscription,
    blocksQueryHandler: BlocksQuery,
    blocksSubHandler:   BlocksSubscription,
    ip:                 String,
    port:               Int,
    webPort:            Int,
    apiKeyHash:         Option[Base58Data]
  )(implicit system:    ActorSystem): F[Unit] =
    for {
      subHandlers <- List(
        TransactionsQueryHandler.partial(txQueryHandler),
        TransactionsSubscriptionHandler.partial(txSubHandler),
        BlocksQueryHandler.partial(blocksQueryHandler),
        BlocksSubscriptionHandler.partial(blocksSubHandler),
        ServerReflection.partial(
          List(TransactionsQuery, BlocksQuery, TransactionsSubscription, BlocksSubscription)
        )
      ).pure[F]
      binding <-
        Async[F].fromFuture(
          Async[F].delay(
            Http(system)
              .newServerAt(ip, port)
              .bind(
                useAuth(apiKeyHash)(handle(ServiceHandler.concatOrNotFound(subHandlers: _*)))
              )
          )
        )
      _ <- Async[F].delay(println(s"Genus gRPC server running: $binding"))
      webBinding <-
        Async[F].fromFuture(
          Async[F].delay(
            Http(system)
              .newServerAt(ip, webPort)
              .bind(
                useAuth(apiKeyHash)(handle(WebHandler.grpcWebHandler(subHandlers: _*)))
              )
          )
        )
      _ <- Async[F].delay(println(s"Genus gRPC-web server running: $webBinding"))
      // run indefinitely
      _ <- Async[F].never[Unit]
    } yield ()
}
