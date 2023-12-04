package co.topl.node

import cats.effect.{IO, Resource}
import cats.implicits._
import co.topl.config.ApplicationConfig
import co.topl.grpc.{HealthCheckGrpc, NodeGrpc}
import co.topl.healthcheck.HealthCheck
import co.topl.node.ApplicationConfigOps._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

class IdleApp(appConfig: ApplicationConfig) {

  type F[+A] = IO[A]

  implicit private val logger: Logger[F] =
    Slf4jLogger.getLoggerFromName[F]("Bifrost.Idle")

  def run: F[Unit] =
    (
      for {
        _ <- Resource.make(Logger[F].info("Launching node in idle operation mode"))(_ => Logger[F].info("Done"))
        _ <- Logger[F].info(show"Node configuration=$appConfig").toResource
        _ <- Resource.make(Logger[F].info("Launching healthcheck"))(_ => Logger[F].info("Healthcheck terminated"))
        healthCheck    <- HealthCheck.make[F]()
        healthServices <- HealthCheckGrpc.Server.services(healthCheck.healthChecker)
        _ <- NodeGrpc.Server.serve[F](appConfig.bifrost.rpc.bindHost, appConfig.bifrost.rpc.bindPort)(healthServices)
        _ <- Logger[F].info(show"Waiting forever...").toResource
        _ <- Resource.never[F, Unit]
      } yield ()
    ).use_

}
