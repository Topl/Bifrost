package co.topl.prometheus

import cats.implicits._
import cats.effect.{Async, Concurrent, LiftIO, Resource, Sync}
import io.opentelemetry.api.GlobalOpenTelemetry
import org.typelevel.otel4s.Otel4s
import org.typelevel.otel4s.java.OtelJava
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

object Otel {

  implicit private def logger[F[_]: Sync]: Logger[F] =
    Slf4jLogger.getLoggerFromName[F]("Otel")

  def otelResource[F[_]: Sync: Async: LiftIO]: Resource[F, Otel4s[F]] =
    Resource
      .eval(Sync[F].delay(GlobalOpenTelemetry.get))
      .evalMap(OtelJava.forAsync[F])
}
