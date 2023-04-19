package co.topl.genus

import cats.effect.kernel.Async
import cats.effect.kernel.Resource
import co.topl.genus.services._
import co.topl.genusLibrary.algebras.BlockFetcherAlgebra
import co.topl.genusLibrary.algebras.TransactionFetcherAlgebra
import fs2.grpc.syntax.all._
import io.grpc.Server
import io.grpc.netty.shaded.io.grpc.netty.NettyServerBuilder
import io.grpc.protobuf.services.ProtoReflectionService

import java.net.InetSocketAddress

object GenusGrpc {

  object Server {

    def serve[F[_]: Async](
      host:                      String,
      port:                      Int,
      blockFetcher:              BlockFetcherAlgebra[F],
      transactionFetcherAlgebra: TransactionFetcherAlgebra[F]
    ): Resource[F, Server] =
      for {
        fullBlockService <- GenusFullBlockServiceFs2Grpc.bindServiceResource(
          new GrpcGenusFullBlockService(blockFetcher)
        )
        transactionService <- TransactionServiceFs2Grpc.bindServiceResource(
          new GrpcTransactionService(transactionFetcherAlgebra)
        )
        server <-
          NettyServerBuilder
            .forAddress(new InetSocketAddress(host, port))
            .addService(fullBlockService)
            .addService(transactionService)
            .addService(ProtoReflectionService.newInstance())
            .resource[F]
            .evalMap(server => Async[F].delay(server.start()))
      } yield server

  }
}
