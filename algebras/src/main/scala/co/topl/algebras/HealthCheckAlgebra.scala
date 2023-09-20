package co.topl.algebras

import grpc.health.v1.{HealthCheckRequest, HealthCheckResponse}

/**
 * HealthCheckAlgebra
 * An interaction layer intended to convey the health status of a blockchain node and its services.
 * @tparam F Effect type
 * @tparam S Health check response container, Ex: Stream, Seq, etc.
 */
trait HealthCheckAlgebra[F[_], S[_]] {

  def check(req: HealthCheckRequest): F[HealthCheckResponse]

  def watch(req: HealthCheckRequest): F[S[HealthCheckResponse]]

}
