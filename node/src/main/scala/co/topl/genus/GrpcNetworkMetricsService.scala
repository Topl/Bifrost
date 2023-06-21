package co.topl.genus

import cats.data.EitherT
import cats.effect.kernel.Async
import co.topl.genus.services._
import co.topl.genusLibrary.algebras.VertexFetcherAlgebra
import io.grpc.Metadata

class GrpcNetworkMetricsService[F[_]: Async](vertexFetcher: VertexFetcherAlgebra[F])
    extends NetworkMetricsServiceFs2Grpc[F, Metadata] {

  def getTxoStats(request: GetTxoStatsReq, ctx: Metadata): F[GetTxoStatsRes] =
    EitherT(vertexFetcher.fetchTxoStats())
      .map(GetTxoStatsRes(_))
      .rethrowT
      .adaptErrorsToGrpc

  def getBlockchainSizeStats(request: BlockchainSizeStatsReq, ctx: Metadata): F[BlockchainSizeStatsRes] =
    EitherT(vertexFetcher.fetchBlockchainSizeStats())
      .map(BlockchainSizeStatsRes(_))
      .rethrowT
      .adaptErrorsToGrpc

  def getBlockStats(request: BlockStatsReq, ctx: Metadata): F[BlockStatsRes] =
    EitherT(vertexFetcher.fetchBlockStats())
      .map(BlockStatsRes(_))
      .rethrowT
      .adaptErrorsToGrpc
}
