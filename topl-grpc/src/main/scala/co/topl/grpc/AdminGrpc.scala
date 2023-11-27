package co.topl.grpc

import cats.effect.kernel.{Async, Resource}
import cats.implicits._
import co.topl.algebras.AdminRpc
import co.topl.grpc.services.AdminService
import co.topl.node.services.NodeAdminRpcFs2Grpc
import fs2.Stream
import fs2.grpc.syntax.all.fs2GrpcSyntaxServerBuilder
import io.grpc.{Server, ServerServiceDefinition}
import io.grpc.netty.shaded.io.grpc.netty.NettyServerBuilder
import io.grpc.protobuf.services.ProtoReflectionService
import java.net.InetSocketAddress

object AdminGrpc {

  object Server {

    def serve[F[_]: Async](host: String, port: Int)(services: List[ServerServiceDefinition]): Resource[F, Server] =
      services
        .foldLeft(
          NettyServerBuilder
            .forAddress(new InetSocketAddress(host, port))
        )(_.addService(_))
        .addService(ProtoReflectionService.newInstance())
        .resource[F]
        .evalMap(server => Async[F].delay(server.start()))

    def services[F[_]: Async](
      interpreter: AdminRpc[F, Stream[F, *]]
    ): Resource[F, List[ServerServiceDefinition]] =
      List(NodeAdminRpcFs2Grpc.bindServiceResource(new AdminService(interpreter))).sequence
  }
}
