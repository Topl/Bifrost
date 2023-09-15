package co.topl.healthcheck

import cats.effect._
import co.topl.algebras._
import fs2.Stream
import fs2.concurrent.SignallingRef
import grpc.health.v1.HealthCheckResponse.ServingStatus
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

/**
 * Captures the interpreters needed to query the health.
 */
case class HealthCheck[F[_], S[_]](
                              healthChecker:      HealthCheckAlgebra[F, Stream[F, *]]
                            )

object HealthCheck {

  def make[F[_]: Async](): Resource[F, HealthCheck[F, fs2.Stream[F, *]]] = {
    val checkRef: F[Ref[F, Map[String, ServingStatus]]] =
    // empty string indicates the health of the server in general,
    // rather than any particular gRPC service running on the server
      Ref.of[F, Map[String, ServingStatus]](Map(
        "" -> ServingStatus.SERVING,
        "Bifrost" -> ServingStatus.SERVING,
        "Genus" -> ServingStatus.SERVING
        )
      )

    val watchSignal: F[SignallingRef[F, Option[ServiceStatus]]] =
      SignallingRef.of[F, Option[ServiceStatus]](None)

    for {
      implicit0(logger: Logger[F]) <- Resource.pure(Slf4jLogger.getLoggerFromName[F]("HealthCheck"))

      ref <- Resource.liftK(checkRef)
      signal <- Resource.liftK(watchSignal)

      healthChecker <- HealthChecker.make[F](ref, signal)
    } yield HealthCheck(healthChecker)
  }
}
