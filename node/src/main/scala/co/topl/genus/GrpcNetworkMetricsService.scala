package co.topl.genus

import cats.data.EitherT
import cats.effect.kernel.Async
import co.topl.genus.services._
import co.topl.genusLibrary.algebras.VertexFetcherAlgebra
import co.topl.genusLibrary.model.GEs
import io.grpc.Metadata

class GrpcNetworkMetricsService[F[_]: Async](vertexFetcher: VertexFetcherAlgebra[F])
    extends NetworkMetricsServiceFs2Grpc[F, Metadata] {

  override def getTxoStats(request: GetTxoStatsReq, ctx: Metadata): F[GetTxoStatsRes] =
    EitherT(vertexFetcher.fetchTxoStats())
      .foldF(
        ge => Async[F].raiseError[GetTxoStatsRes](GEs.Internal(ge)),
        stats => Async[F].delay(GetTxoStatsRes(stats))
      )
      .adaptErrorsToGrpc
}
