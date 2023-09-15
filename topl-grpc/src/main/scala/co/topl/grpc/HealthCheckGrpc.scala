package co.topl.grpc

import cats.effect.kernel.{Async, Resource}
import cats.implicits._
import co.topl.algebras.HealthCheckAlgebra
import co.topl.grpc.services.HealthCheckService
import fs2.Stream
import grpc.health.v1._
import io.grpc.ServerServiceDefinition

object HealthCheckGrpc {
  object Server {

    def services[F[_] : Async](
      healthCheck: HealthCheckAlgebra[F, Stream[F, *]]
    ): Resource[F, List[ServerServiceDefinition]] =
      List(
        HealthFs2Grpc.bindServiceResource(new HealthCheckService(healthCheck))
      ).sequence
  }
}
