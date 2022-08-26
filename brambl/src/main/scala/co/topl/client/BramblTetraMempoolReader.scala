package co.topl.client

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import cats.effect.{Async, IO, Sync}
import cats.implicits._
import co.topl.algebras.ToplRpc
import co.topl.catsakka._
import co.topl.grpc.ToplGrpc
import co.topl.typeclasses.implicits._
import com.typesafe.config.ConfigFactory
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration._

object BramblTetraMempoolReader
    extends IOAkkaApp[Unit, Nothing](
      _ => (),
      _ => ConfigFactory.load(),
      (_, config) => ActorSystem[Nothing](Behaviors.empty, "BramblTetraMempoolReader", config)
    ) {

  override def run: IO[Unit] =
    ToplGrpc.Client
      .make[F]("localhost", 9084, tls = false)
      .flatMap(implicit rpcClient =>
        Slf4jLogger
          .fromName[F]("Brambl@localhost:9084")
          .flatMap(implicit logger => infiniteMempool(1500.milli))
      )
      .void

  private def infiniteMempool(sleepDuration: FiniteDuration)(implicit rpcClient: ToplRpc[F], logger: Logger[F]) =
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
