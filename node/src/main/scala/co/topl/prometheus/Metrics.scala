package co.topl.prometheus

import cats.effect._
import cats.implicits._
import cats.effect.implicits._
import io.chrisdavenport.epimetheus._
import io.chrisdavenport.epimetheus.implicits._
import cats.effect.unsafe.implicits.global
import org.typelevel.log4cats._
import co.topl.config.ApplicationConfig
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.http4s.Response
import org.http4s.HttpRoutes
import cats.Functor
import org.http4s.dsl.Http4sDsl
import org.http4s.Status
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits._
import org.http4s.server.middleware.Logger
import com.comcast.ip4s._
import fs2.io.net.Network
import org.http4s.HttpApp

object Metrics {

  // Create a new Prometheus registry and launch the metrics server.
  def make[F[_]: Async](appConfig: ApplicationConfig): Resource[F, PrometheusRegistry[F]] =
    for {
      pr <- PrometheusRegistry.buildWithDefaults[F].toResource
    } yield (pr)
}

object ExporterRoutes {

  def response[F[_]: Functor](cr: PrometheusRegistry[F]): F[Response[F]] =
    cr.write004.map(Response[F](Status.Ok).withEntity(_))

  def buildRoutes[F[_]: Sync](cr: PrometheusRegistry[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F] {}; import dsl._
    HttpRoutes.of[F] { case GET -> Root / "metrics" =>
      response(cr)
    }
  }
}

object PrometheusExportServer {

  def make[F[_]: Async](appConfig: ApplicationConfig, prometheusRegistry: PrometheusRegistry[F]): Resource[F, Unit] =
    for {
      httpServer <- {
        for {
          routes <- ExporterRoutes.buildRoutes[F](prometheusRegistry)
          httpApp = HttpApp[F](routes)
        } yield EmberServerBuilder
          .default[F]
          .withHost(ipv4"0.0.0.0")
          .withPort(port"8080")
          .withHttpApp(httpApp)
          .build
      }.toResource
    } yield (httpServer)
}
