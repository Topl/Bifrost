package co.topl.client

import akka.NotUsed
import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.stream.scaladsl.Source
import cats.effect.{Async, IO, IOApp, Sync}
import cats.implicits._
import co.topl.algebras.ToplRpc
import co.topl.catsakka.AkkaCatsRuntime
import co.topl.grpc.ToplGrpc
import co.topl.models._
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration._

object BramblTetraMempoolReader extends IOApp.Simple {

  type F[A] = IO[A]

  override def run: IO[Unit] =
    AkkaCatsRuntime
      .systemResource[F, Nothing](ActorSystem(Behaviors.empty, "Brambl"))
      .use(implicit system =>
        ToplGrpc.Client
          .make[F]("localhost", 8090, tls = false)
          .flatMap(implicit rpcClient =>
            Slf4jLogger
              .fromName[F]("Brambl@localhost:8090")
              .flatMap(implicit logger => infiniteMempool(1500.milli))
          )
          .parProduct(
            Async[F].sleep(200.milli) >>
            ToplGrpc.Client
              .make[F]("localhost", 8091, tls = false)
              .flatMap(implicit rpcClient =>
                Slf4jLogger
                  .fromName[F]("Brambl@localhost:8091")
                  .flatMap(implicit logger => infiniteMempool(1500.milli))
              )
          )
          .void
      )

  private def infiniteMempool(
    sleepDuration:      FiniteDuration
  )(implicit rpcClient: ToplRpc[F, Source[*, NotUsed]], logger: Logger[F]) =
    Sync[F]
      .defer(
        for {
          mempool <- rpcClient.currentMempool()
          _       <- Logger[F].info(show"Current mempool=$mempool")
          _       <- Async[F].sleep(sleepDuration)
        } yield ()
      )
      .foreverM
      .void

}
