package co.topl.genus

import cats.effect.kernel.Async
import cats.effect.kernel.Resource
import cats.implicits._
import cats.Eval
import cats.Now
import co.topl.algebras.ToplGenusRpc
import co.topl.genus.services._
import co.topl.genusLibrary.algebras.BlockFetcherAlgebra
import co.topl.genusLibrary.algebras.TransactionFetcherAlgebra
import co.topl.genusLibrary.algebras.VertexFetcherAlgebra
import fs2.grpc.syntax.all._
import io.grpc.Metadata
import io.grpc.ServerServiceDefinition
import io.grpc.netty.shaded.io.grpc.netty.NettyChannelBuilder

object GenusGrpc {

  object Client {

    /**
     * Creates a Genus RPC Client for interacting with a Bifrost node
     *
     * @param host Genus node host/IP
     * @param port Genus node port
     * @param tls  Should the connection use TLS?
     */
    def make[F[_]: Async](host: String, port: Int, tls: Boolean): Resource[F, ToplGenusRpc[F]] =
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
        .flatMap(BlockServiceFs2Grpc.stubResource[F])
        .map(client =>
          new ToplGenusRpc[F] {

            override def blockIdAtHeight(height: Long): F[BlockResponse] =
              client.getBlockByHeight(
                GetBlockByHeightRequest(
                  ChainDistance(height),
                  confidenceFactor = None
                ),
                new Metadata()
              )
          }
        )

  }

  object Server {

    def services[F[_]: Async](
      blockFetcher:       BlockFetcherAlgebra[F],
      transactionFetcher: TransactionFetcherAlgebra[F],
      vertexFetcher:      VertexFetcherAlgebra[F]
    ): Resource[F, List[ServerServiceDefinition]] =
      List(
        BlockServiceFs2Grpc.bindServiceResource(new GrpcBlockService(blockFetcher)),
        TransactionServiceFs2Grpc.bindServiceResource(new GrpcTransactionService(transactionFetcher)),
        NetworkMetricsServiceFs2Grpc.bindServiceResource(new GrpcNetworkMetricsService(vertexFetcher))
      ).sequence

  }
}
