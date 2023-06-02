package co.topl.grpc

import cats.effect.kernel.Async
import cats.effect.kernel.Resource
import fs2.grpc.syntax.all._
import io.grpc.Server
import io.grpc.ServerServiceDefinition
import io.grpc.netty.shaded.io.grpc.netty.NettyServerBuilder
import io.grpc.protobuf.services.ProtoReflectionService

import java.net.InetSocketAddress

object ToplGrpc {

  object Server {

    /**
     * Serves the given gRPC Services
     * @param host The host to bind
     * @param port The port to bind
     * @param services The gRPC services to launch
     */
    def serve[F[_]: Async](host: String, port: Int)(services: List[ServerServiceDefinition]): Resource[F, Server] =
      services
        .foldLeft(
          NettyServerBuilder
            .forAddress(new InetSocketAddress(host, port))
        )(_.addService(_))
        .addService(ProtoReflectionService.newInstance())
        .resource[F]
        .evalMap(server => Async[F].delay(server.start()))
  }
}
