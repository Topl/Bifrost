package co.topl.genus

import cats.MonadThrow
import cats.effect.implicits._
import cats.effect.{Async, Ref, Resource}
import cats.implicits._
import co.topl.algebras.HealthCheckAlgebra
import co.topl.models.ServiceStatus
import co.topl.typeclasses.implicits._
import fs2.Stream
import fs2.concurrent.SignallingRef
import grpc.health.v1.{HealthCheckRequest, HealthCheckResponse, ServingStatus}
import io.grpc.{Status, StatusException}

/**
 * HealthCheck
 * Captures the interpreters needed to query the health check service.
 * @tparam F Effect type
 * @tparam S Health check response container, Ex: Stream, Seq, etc.
 */
case class GenusHealthCheck[F[_], S[_]](
  healthChecker: HealthCheckAlgebra[F, Stream[F, *]]
)

object GenusHealthCheck {

  def make[F[_]: Async](): Resource[F, GenusHealthCheck[F, fs2.Stream[F, *]]] =
    for {
      ref <- Ref
        .of[F, Map[String, ServingStatus]](
          Map(
            ""      -> ServingStatus.SERVING,
            "Genus" -> ServingStatus.SERVING
          )
        )
        .toResource
      signal <- SignallingRef.of[F, Option[ServiceStatus]](None).toResource

      healthChecker <- HealthChecker.make[F](ref, signal)
    } yield GenusHealthCheck(healthChecker)
}

object HealthChecker {

  def make[F[_]: Async](
    checkRef:    Ref[F, Map[String, ServingStatus]],
    watchSignal: SignallingRef[F, Option[ServiceStatus]]
  ): Resource[F, HealthCheckAlgebra[F, Stream[F, *]]] =
    Resource.pure {
      new HealthCheckAlgebra[F, Stream[F, *]] {

        private def getStatus(service: String): F[Option[ServingStatus]] =
          checkRef.get.map(_.get(service))

        override def check(request: HealthCheckRequest): F[HealthCheckResponse] =
          getStatus(request.service).flatMap {
            case Some(status) =>
              HealthCheckResponse(status).pure[F]
            case None =>
              MonadThrow[F].raiseError(new StatusException(Status.NOT_FOUND))
          }

        override def watch(request: HealthCheckRequest): F[Stream[F, HealthCheckResponse]] = {
          val currentStatus =
            Stream.eval(getStatus(request.service).map(_.getOrElse(ServingStatus.SERVICE_UNKNOWN)))
          val futureStatuses = watchSignal.discrete
            .collect { case Some(x) => x }
            .filter(_.service === request.service)
            .map(_.status)

          (currentStatus ++ futureStatuses).changes
            .map(HealthCheckResponse(_))
            .pure[F]
        }
      }
    }
}
