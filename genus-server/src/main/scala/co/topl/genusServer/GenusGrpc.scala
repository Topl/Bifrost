package co.topl.genusServer

import cats.{Eval, MonadThrow, Now}
import cats.effect.kernel.{Async, Resource}
import io.grpc.{Metadata, Server}
import cats.implicits._
import fs2.grpc.syntax.all._
import io.grpc.netty.shaded.io.grpc.netty.{NettyChannelBuilder, NettyServerBuilder}
import io.grpc.protobuf.services.ProtoReflectionService
import co.topl.proto.genus.{
  BlockResponse,
  GenusFullBlockServiceFs2Grpc,
  GetBlockByDepthRequest,
  GetBlockByHeightRequest,
  GetBlockByIdRequest
}
import java.net.InetSocketAddress

object GenusGrpc {

  object Client {

    /**
     * Creates a Genus RPC Client for interacting with a Bifrost node
     *
     * @param host Bifrost node host/IP
     * @param port Bifrost node port
     * @param tls  Should the connection use TLS?
     */
    def make[F[_]: Async](host: String, port: Int, tls: Boolean): Resource[F, GenusRpc[F]] =
      Eval
        .now(NettyChannelBuilder.forAddress(host, port))
        .flatMap(ncb =>
          Eval
            .now(tls)
            .ifM(
              Now(ncb.useTransportSecurity()),
              Now(ncb.usePlaintext())
            )
        )
        .value
        .resource[F]
        .flatMap(GenusFullBlockServiceFs2Grpc.stubResource[F])
        .map(client =>
          new GenusRpc[F] {
            def helloWorld(): F[String] = "helloWorld".pure[F]
          }
        )

  }

  object Server {

    def serve[F[_]: Async](host: String, port: Int, interpreter: GenusRpc[F]): Resource[F, Server] =
      GenusFullBlockServiceFs2Grpc
        .bindServiceResource(
          new GrpcServerImpl(interpreter)
        )
        .flatMap(serverServiceDefinition =>
          NettyServerBuilder
            .forAddress(new InetSocketAddress(host, port))
            .addService(serverServiceDefinition)
            .addService(ProtoReflectionService.newInstance())
            .resource[F]
            .evalMap(server => Async[F].delay(server.start()))
        )

    private class GrpcServerImpl[F[_]: MonadThrow](interpreter: GenusRpc[F])
        extends GenusFullBlockServiceFs2Grpc[F, Metadata] {
      override def getBlockById(request: GetBlockByIdRequest, ctx: Metadata): F[BlockResponse] = ???

      override def getBlockByHeight(request: GetBlockByHeightRequest, ctx: Metadata): F[BlockResponse] = ???

      override def getBlockByDepth(request: GetBlockByDepthRequest, ctx: Metadata): F[BlockResponse] = ???
    }

  }

}
