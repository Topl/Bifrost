package co.topl.genusServer

import cats.data.EitherT
import cats.effect.kernel.Async
import cats.effect.kernel.Resource
import cats.implicits._
import co.topl.consensus.models.BlockHeader
import co.topl.genus.services._
import co.topl.genusLibrary.algebras.BlockFetcherAlgebra
import co.topl.genusLibrary.model.GenusExceptions
import co.topl.node.models.{FullBlock, FullBlockBody}
import fs2.grpc.syntax.all._
import io.grpc.netty.shaded.io.grpc.netty.NettyServerBuilder
import io.grpc.protobuf.services.ProtoReflectionService
import io.grpc.{Metadata, Server}
import java.net.InetSocketAddress

object GenusFullBlockGrpc {

  object Server {

    def serve[F[_]: Async](host: String, port: Int, blockFetcher: BlockFetcherAlgebra[F]): Resource[F, Server] =
      GenusFullBlockServiceFs2Grpc
        .bindServiceResource(
          new GrpcServerImpl(blockFetcher)
        )
        .flatMap(serverServiceDefinition =>
          NettyServerBuilder
            .forAddress(new InetSocketAddress(host, port))
            .addService(serverServiceDefinition)
            .addService(ProtoReflectionService.newInstance())
            .resource[F]
            .evalMap(server => Async[F].delay(server.start()))
        )

    private class GrpcServerImpl[F[_]: Async](blockFetcher: BlockFetcherAlgebra[F])
        extends GenusFullBlockServiceFs2Grpc[F, Metadata] {

      override def getBlockById(request: GetBlockByIdRequest, ctx: Metadata): F[BlockResponse] =
        (for {
          header <- EitherT(blockFetcher.fetchHeader(request.blockId))
            .foldF(
              ge => Async[F].raiseError[BlockHeader](GenusExceptions.Internal(ge)),
              {
                case Some(blockHeader) =>
                  blockHeader.pure[F]
                case None =>
                  Async[F].raiseError[BlockHeader](GenusExceptions.NotFound("Block not found"))
              }
            )

          // TODO populate transaction first, and then implement vertexFetchBody, vertexFetchTransactions
          response = BlockResponse.of(FullBlock.of(header, fullBody = FullBlockBody.defaultInstance))
        } yield response).adaptErrorsToGrpc

      override def getBlockByHeight(request: GetBlockByHeightRequest, ctx: Metadata): F[BlockResponse] =
        (for {
          blockData <- EitherT(
            blockFetcher.fetchBlockByHeight(request.height.value)
          )
            .foldF(
              ge => Async[F].raiseError[BlockData](GenusExceptions.Internal(ge)),
              {
                case Some(blockHeader) =>
                  blockHeader.pure[F]
                case None =>
                  Async[F].raiseError[BlockData](GenusExceptions.NotFound("Block not found"))
              }
            )

          response = BlockResponse.of(
            FullBlock.of(blockData.header, fullBody = FullBlockBody.of(blockData.transactions))
          )
        } yield response).adaptErrorsToGrpc

      override def getBlockByDepth(request: GetBlockByDepthRequest, ctx: Metadata): F[BlockResponse] =
        Async[F].raiseError[BlockResponse](GenusExceptions.UnImplemented).adaptErrorsToGrpc

    }

  }

}
