package co.topl.genusServer

import cats.data.EitherT
import cats.effect.kernel.Async
import cats.effect.kernel.Resource
import cats.implicits._
import co.topl.consensus.models.BlockHeader
import co.topl.genus.services._
import co.topl.genusLibrary.algebras.VertexFetcherAlgebra
import co.topl.node.models.{FullBlock, FullBlockBody}
import fs2.grpc.syntax.all._
import io.grpc.netty.shaded.io.grpc.netty.NettyServerBuilder
import io.grpc.protobuf.services.ProtoReflectionService
import io.grpc.{Metadata, Server, Status}
import java.net.InetSocketAddress

object GenusFullBlockGrpc {

  object Server {

    def serve[F[_]: Async](host: String, port: Int, vertexFetcher: VertexFetcherAlgebra[F]): Resource[F, Server] =
      GenusFullBlockServiceFs2Grpc
        .bindServiceResource(
          new GrpcServerImpl(vertexFetcher)
        )
        .flatMap(serverServiceDefinition =>
          NettyServerBuilder
            .forAddress(new InetSocketAddress(host, port))
            .addService(serverServiceDefinition)
            .addService(ProtoReflectionService.newInstance())
            .resource[F]
            .evalMap(server => Async[F].delay(server.start()))
        )

    private class GrpcServerImpl[F[_]: Async](vertexFetcher: VertexFetcherAlgebra[F])
        extends GenusFullBlockServiceFs2Grpc[F, Metadata] {

      override def getBlockById(request: GetBlockByIdRequest, ctx: Metadata): F[BlockResponse] =
        for {
          header <- EitherT(vertexFetcher.fetchHeader(request.blockId))
            .foldF(
              ge =>
                Async[F].raiseError[BlockHeader](
                  Status.INTERNAL.augmentDescription(s"${ge.getMessage}").asRuntimeException()
                ),
              {
                case Some(blockHeader) =>
                  blockHeader.pure[F]
                case None =>
                  // TODO return None when optional response PR is approved
                  Async[F].raiseError[BlockHeader](
                    Status.NOT_FOUND.augmentDescription("Block header not found").asRuntimeException()
                  )
              }
            )

          // TODO populate transaction first, and then implement vertexFetchBody, vertexFetchTransactions
          response = BlockResponse.of(FullBlock.of(header, fullBody = FullBlockBody.defaultInstance))
        } yield response

      override def getBlockByHeight(request: GetBlockByHeightRequest, ctx: Metadata): F[BlockResponse] =
        for {
          header <- EitherT(vertexFetcher.fetchHeaderByHeight(request.height.value))
            .foldF(
              ge =>
                Async[F].raiseError[BlockHeader](
                  Status.INTERNAL.augmentDescription(s"${ge.getMessage}").asRuntimeException()
                ), // TODO return None
              {
                case Some(blockHeader) => blockHeader.pure[F]
                case None              =>
                  // TODO return None when optional response PR is approved
                  Async[F].raiseError[BlockHeader](
                    Status.NOT_FOUND.augmentDescription("Block header not found").asRuntimeException()
                  )
              }
            )
          // TODO populate transaction first, and then implement vertexFetchBody, vertexFetchTransactions
          response = BlockResponse.of(FullBlock.of(header, fullBody = FullBlockBody.defaultInstance))
        } yield response

      override def getBlockByDepth(request: GetBlockByDepthRequest, ctx: Metadata): F[BlockResponse] =
        Async[F].raiseError(
          Status.UNIMPLEMENTED.asException()
        )

    }

  }

}
