package co.topl.grpc.services

import cats.effect.Async
import cats.implicits.toFunctorOps
import co.topl.algebras.HealthCheckAlgebra
import co.topl.grpc.FApplicativeErrorAdapter
import fs2.Stream
import grpc.health.v1.{HealthCheckRequest, HealthCheckResponse, HealthFs2Grpc}
import io.grpc.Metadata

class HealthCheckService[F[_]: Async](healthCheck: HealthCheckAlgebra[F, Stream[F, *]])
  extends HealthFs2Grpc[F, Metadata] {
  def check(request: HealthCheckRequest, ctx: Metadata): F[HealthCheckResponse] =
    healthCheck
      .check(request)
      .adaptErrorsToGrpc

  def watch(request: HealthCheckRequest, ctx: Metadata): Stream[F, HealthCheckResponse] =
    Stream
      .force(healthCheck.watch(request))
      .adaptErrorsToGrpc
}
