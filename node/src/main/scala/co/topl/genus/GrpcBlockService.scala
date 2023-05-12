package co.topl.genus

import cats.data.EitherT
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.genus.services._
import co.topl.genusLibrary.algebras.BlockFetcherAlgebra
import co.topl.genusLibrary.model.GEs
import co.topl.node.models.FullBlock
import co.topl.node.models.FullBlockBody
import co.topl.typeclasses.implicits._
import io.grpc.Metadata

class GrpcBlockService[F[_]: Async](blockFetcher: BlockFetcherAlgebra[F]) extends BlockServiceFs2Grpc[F, Metadata] {

  override def getBlockById(request: GetBlockByIdRequest, ctx: Metadata): F[BlockResponse] =
    EitherT(blockFetcher.fetchBlock(request.blockId))
      .foldF(
        ge => Async[F].raiseError[BlockData](GEs.Internal(ge)),
        _.map(_.pure[F])
          .getOrElse(
            Async[F]
              .raiseError[BlockData](GEs.NotFound(s"BlockId:${request.blockId.show}"))
          )
      )
      .map(blockData => BlockResponse.of(FullBlock.of(blockData.header, FullBlockBody.of(blockData.transactions))))
      .adaptErrorsToGrpc

  override def getBlockByHeight(request: GetBlockByHeightRequest, ctx: Metadata): F[BlockResponse] =
    EitherT(blockFetcher.fetchBlockByHeight(request.height.value))
      .foldF(
        ge => Async[F].raiseError[BlockData](GEs.Internal(ge)),
        _.map(_.pure[F])
          .getOrElse(
            Async[F].raiseError[BlockData](GEs.NotFound(s"Height:${request.height.value.show}"))
          )
      )
      .map(blockData => BlockResponse.of(FullBlock.of(blockData.header, FullBlockBody.of(blockData.transactions))))
      .adaptErrorsToGrpc

  override def getBlockByDepth(request: GetBlockByDepthRequest, ctx: Metadata): F[BlockResponse] =
    EitherT(blockFetcher.fetchBlockByDepth(request.depth.value))
      .foldF(
        ge => Async[F].raiseError[BlockData](GEs.Internal(ge)),
        _.map(_.pure[F])
          .getOrElse(
            Async[F].raiseError[BlockData](GEs.NotFound(s"Depth:${request.depth.value.show}"))
          )
      )
      .map(blockData => BlockResponse.of(FullBlock.of(blockData.header, FullBlockBody.of(blockData.transactions))))
      .adaptErrorsToGrpc

}
