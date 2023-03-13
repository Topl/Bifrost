package co.topl.genusServer

import cats.{Eval, MonadThrow, Now}
import cats.effect.kernel.{Async, Resource}
import io.grpc.{Metadata, Server}
import cats.implicits._
import co.topl.genus.services._
import co.topl.genusLibrary.algebras.BlockFetcherAlgebra
import co.topl.node.models.{FullBlock, FullBlockBody}
import fs2.grpc.syntax.all._
import io.grpc.netty.shaded.io.grpc.netty.{NettyChannelBuilder, NettyServerBuilder}
import io.grpc.protobuf.services.ProtoReflectionService
import java.net.InetSocketAddress

object GenusFullBlockGrpc {

//  object Client { // TODO double check if we need a client
//
//    /**
//     * Creates a Genus RPC Client for interacting with a Bifrost node
//     *
//     * @param host Bifrost node host/IP
//     * @param port Bifrost node port
//     * @param tls  Should the connection use TLS?
//     */
//    def make[F[_]: Async](host: String, port: Int, tls: Boolean): Resource[F, GenusRpc[F]] =
//      Eval
//        .now(NettyChannelBuilder.forAddress(host, port))
//        .flatMap(ncb =>
//          Eval
//            .now(tls)
//            .ifM(
//              Now(ncb.useTransportSecurity()),
//              Now(ncb.usePlaintext())
//            )
//        )
//        .value
//        .resource[F]
//        .flatMap(GenusFullBlockServiceFs2Grpc.stubResource[F])
//        .map(client =>
//          new GenusRpc[F] {
//            def helloWorld(): F[String] = "helloWorld".pure[F]
//          }
//        )
//
//  }

  object Server {

    def serve[F[_]: Async](host: String, port: Int, blockFetcherAlgebra: BlockFetcherAlgebra[F]): Resource[F, Server] =
      GenusFullBlockServiceFs2Grpc
        .bindServiceResource(
          new GrpcServerImpl(blockFetcherAlgebra)
        )
        .flatMap(serverServiceDefinition =>
          NettyServerBuilder
            .forAddress(new InetSocketAddress(host, port))
            .addService(serverServiceDefinition)
            .addService(ProtoReflectionService.newInstance())
            .resource[F]
            .evalMap(server => Async[F].delay(server.start()))
        )

    private class GrpcServerImpl[F[_]: MonadThrow](blockFetcherAlgebra: BlockFetcherAlgebra[F])
        extends GenusFullBlockServiceFs2Grpc[F, Metadata] {

      override def getBlockById(request: GetBlockByIdRequest, ctx: Metadata): F[BlockResponse] =
        ???

      /**
       * GetBlockByHeight
       * @param request
       * @param ctx
       * @return
       */
      override def getBlockByHeight(request: GetBlockByHeightRequest, ctx: Metadata): F[BlockResponse] =
        blockFetcherAlgebra
          .fetch(request.height.value)
          .map(
            _.fold(
              _ => BlockResponse.defaultInstance,
              heightData =>
                heightData.blockData match {
                  case Some(blockData) =>
                    BlockResponse.of(
                      FullBlock.of(
                        header = blockData.header,
                        fullBody =  FullBlockBody.of(blockData.transactions.toList)
                      )
                    )
                  case None => BlockResponse.defaultInstance
                }
            )
          )

      override def getBlockByDepth(request: GetBlockByDepthRequest, ctx: Metadata): F[BlockResponse] = ???
    }

  }

}
