package co.topl.genusServer

import cats.data.EitherT
import cats.effect.kernel.Async
import cats.effect.kernel.Resource
import cats.implicits._
import co.topl.genus.services._
import co.topl.genusLibrary.algebras.BlockFetcherAlgebra
import co.topl.genusLibrary.model.GREs
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

    private[genusServer] class GrpcServerImpl[F[_]: Async](blockFetcher: BlockFetcherAlgebra[F])
        extends GenusFullBlockServiceFs2Grpc[F, Metadata] {

      override def getBlockById(request: GetBlockByIdRequest, ctx: Metadata): F[BlockResponse] =
        EitherT(blockFetcher.fetchBlock(request.blockId))
          .foldF(
            ge => Async[F].raiseError[BlockData](GREs.Internal(ge)),
            _.map(_.pure[F])
              .getOrElse(Async[F].raiseError[BlockData](GREs.NotFound("Block not found")))
          )
          .map(blockData => BlockResponse.of(FullBlock.of(blockData.header, FullBlockBody.of(blockData.transactions))))
          .adaptErrorsToGrpc

      override def getBlockByHeight(request: GetBlockByHeightRequest, ctx: Metadata): F[BlockResponse] =
        EitherT(blockFetcher.fetchBlockByHeight(request.height.value))
          .foldF(
            ge => Async[F].raiseError[BlockData](GREs.Internal(ge)),
            _.map(_.pure[F])
              .getOrElse(Async[F].raiseError[BlockData](GREs.NotFound("Block not found")))
          )
          .map(blockData => BlockResponse.of(FullBlock.of(blockData.header, FullBlockBody.of(blockData.transactions))))
          .adaptErrorsToGrpc

      override def getBlockByDepth(request: GetBlockByDepthRequest, ctx: Metadata): F[BlockResponse] =
        EitherT(blockFetcher.fetchBlockByDepth(request.depth.value))
          .foldF(
            ge => Async[F].raiseError[BlockData](GREs.Internal(ge)),
            _.map(_.pure[F])
              .getOrElse(Async[F].raiseError[BlockData](GREs.NotFound("Block not found")))
          )
          .map(blockData => BlockResponse.of(FullBlock.of(blockData.header, FullBlockBody.of(blockData.transactions))))
          .adaptErrorsToGrpc

    }
  }
}
