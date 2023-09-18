package co.topl.healthcheck

import cats.{Eq, MonadThrow}
import cats.effect.{Async, Ref, Resource}
import cats.implicits.{catsSyntaxApplicativeId, catsSyntaxEq, toFlatMapOps, toFunctorOps}
import co.topl.algebras.HealthCheckAlgebra
import co.topl.models.ServiceStatus
import fs2.Stream
import fs2.concurrent.SignallingRef
import grpc.health.v1.{HealthCheckRequest, HealthCheckResponse}
import grpc.health.v1.ServingStatus
import io.grpc.{Status, StatusException}
import co.topl.typeclasses.implicits._

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
