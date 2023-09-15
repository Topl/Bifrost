package co.topl.algebras

import grpc.health.v1.{HealthCheckRequest, HealthCheckResponse}

trait HealthCheckAlgebra[F[_], S[_]] {

  def check(req: HealthCheckRequest): F[HealthCheckResponse]

  def watch(req: HealthCheckRequest): F[S[HealthCheckResponse]]

}
