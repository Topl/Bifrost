package co.topl.prometheus

import cats.implicits._
import cats.effect.implicits._
import cats.effect.{Async, Concurrent, LiftIO, Resource, Sync}
import io.opentelemetry.api.GlobalOpenTelemetry
import org.typelevel.otel4s.Otel4s
import org.typelevel.otel4s.java.OtelJava
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.otel4s.metrics.ObservableCounter
// import org.typelevel.otel4s.oteljava.OtelJava
// import org.typelevel.otel4s.sdk.OpenTelemetrySdk

import java.lang.management.ManagementFactory
import javax.management.MBeanServer
import javax.management.ObjectName
import cats.effect.IO
import org.typelevel.otel4s.trace.Tracer
import org.typelevel.otel4s.metrics.Meter

object Otel {

  implicit private def logger[F[_]: Sync]: Logger[F] =
    Slf4jLogger.getLoggerFromName[F]("Otel")

  val mbeanServer: MBeanServer = ManagementFactory.getPlatformMBeanServer
  val mbeanName = new ObjectName("cats.effect.metrics:type=CpuStarvation")

  // def registerMbeanMetrics[F[_]: Sync: Async: LiftIO]: Any =
  //   for {
  //     _ <- otelResource
  //       .evalMap(_.meterProvider.get("observable-example"))
  //       .flatMap(
  //         _.observableCounter[Long]("cats-effect-runtime-cpu-starvation-count")
  //           .withDescription("CE runtime starvation count")
  //           .createWithCallback(obs =>
  //             IO(
  //               mbeanServer
  //                 .getAttribute(mbeanName, "CpuStarvationCount")
  //                 .asInstanceOf[Long]
  //             ).flatMap(c => obs.record(c))
  //           )
  //       )
  //   } yield ()

  // def withMetrics[A](name: String)(f: IO[A]): IO[A] = {
  //   val start = IO.realTime
  //   for {
  //     startTime <- start
  //     result <- f.attempt
  //     endTime <- IO.realTime
  //     duration = (endTime - startTime).toMillis.millis
  //     _ <- result match {
  //       case Left(_) => IO(println(s"Metric $name: failed after $duration"))
  //       case Right(_) => IO(println(s"Metric $name: succeeded after $duration"))
  //     }
  //     res <- IO.fromEither(result)
  //   } yield res
  // }

  def otelResource[F[_]: Sync: Async: LiftIO]: Resource[F, Otel4s[F]] =
    Resource
      .eval(Sync[F].delay(GlobalOpenTelemetry.get))
      .evalMap(OtelJava.forAsync[F])

  def generateProviders[F[_]: Sync: Async: LiftIO](providerName: String): Resource[F, (Meter[F], Tracer[F])] =
    otelResource.evalMap(otel =>
      for {
        meter  <- otel.meterProvider.get(providerName)
        tracer <- otel.tracerProvider.get(providerName)
      } yield (meter, tracer)
    )
}
