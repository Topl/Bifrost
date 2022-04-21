package co.topl.genus.programs

import akka.actor.ActorSystem
import akka.grpc.scaladsl.{ServerReflection, ServiceHandler}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpRequest, HttpResponse}
import akka.http.scaladsl.server.Directive0
import akka.http.scaladsl.server.Directives.{handle, optionalHeaderValueByName, pass, reject}
import cats.effect.kernel.Async
import cats.implicits._
import cats.{~>, Monad}
import co.topl.crypto.hash.blake2b256
import co.topl.genus.algebras._
import co.topl.genus.interpreters.requesthandlers._
import co.topl.genus.services.blocks_query.{BlocksQuery, BlocksQueryHandler}
import co.topl.genus.services.blocks_subscription.{BlocksSubscription, BlocksSubscriptionHandler}
import co.topl.genus.services.transactions_query.{TransactionsQuery, TransactionsQueryHandler}
import co.topl.genus.services.transactions_subscription.{TransactionsSubscription, TransactionsSubscriptionHandler}
import co.topl.utils.implicits._

import scala.concurrent.Future

object GenusProgram {

  def useAuth(apiKeyHash: Option[String]): Directive0 =
    optionalHeaderValueByName("x-api-key").flatMap { keyOpt =>
      (keyOpt, apiKeyHash) match {
        case (Some(provided), Some(needed)) =>
          // check that the provided key hashes to the expected value
          val hashedKey = blake2b256.hash(provided.getBytes("UTF-8"))

          if (hashedKey.value === needed.getBytes()) pass
          else reject

        case (None, Some(_)) => reject
        case _               => pass
      }
    }

  def make[F[_]: Async: Monad: *[_] ~> Future](
    txQuery:         TransactionsQueryService[F],
    txSub:           TransactionsSubscriptionService[F],
    blocksQuery:     BlocksQueryService[F],
    blocksSub:       BlocksSubscriptionService[F],
    ip:              String,
    port:            Int,
    apiKeyHash:      Option[String]
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
            Http(system).newServerAt(ip, port).bind(useAuth(apiKeyHash)(handle(handlers)))
          )
        )
      _ <- Async[F].delay(println(s"Genus server running: $binding"))
      // run indefinitely
      _ <- Async[F].never[Unit]
    } yield ()
}
