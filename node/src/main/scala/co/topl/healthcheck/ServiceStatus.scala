package co.topl.healthcheck

import cats.kernel.Eq
import grpc.health.v1.HealthCheckResponse.ServingStatus

final case class ServiceStatus(service: String, status: ServingStatus)
object ServiceStatus {
  implicit val eq: Eq[ServiceStatus] = Eq.fromUniversalEquals[ServiceStatus]
}
