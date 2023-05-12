package co.topl.genus

import cats.effect.kernel.Async
import cats.effect.kernel.Resource
import co.topl.genus.services._
import co.topl.genusLibrary.algebras.{BlockFetcherAlgebra, TransactionFetcherAlgebra, VertexFetcherAlgebra}
import fs2.grpc.syntax.all._
import io.grpc.Server
import io.grpc.netty.shaded.io.grpc.netty.NettyServerBuilder
import io.grpc.protobuf.services.ProtoReflectionService
import java.net.InetSocketAddress

object GenusGrpc {

  object Server {

    def serve[F[_]: Async](
      host:               String,
      port:               Int,
      blockFetcher:       BlockFetcherAlgebra[F],
      transactionFetcher: TransactionFetcherAlgebra[F],
      vertexFetcher:      VertexFetcherAlgebra[F]
    ): Resource[F, Server] =
      for {
        blockService <- BlockServiceFs2Grpc.bindServiceResource(
          new GrpcBlockService(blockFetcher)
        )
        transactionService <- TransactionServiceFs2Grpc.bindServiceResource(
          new GrpcTransactionService(transactionFetcher)
        )
        networkMetricsService <- NetworkMetricsServiceFs2Grpc.bindServiceResource(
          new GrpcNetworkMetricsService(vertexFetcher)
        )

        server <-
          NettyServerBuilder
            .forAddress(new InetSocketAddress(host, port))
            .addService(blockService)
            .addService(transactionService)
            .addService(networkMetricsService)
            .addService(ProtoReflectionService.newInstance())
            .resource[F]
            .evalMap(server => Async[F].delay(server.start()))
      } yield server

  }
}
