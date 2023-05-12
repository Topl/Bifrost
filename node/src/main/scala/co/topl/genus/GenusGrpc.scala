package co.topl.genus

import cats.{Eval, Now}
import cats.effect.kernel.Async
import cats.effect.kernel.Resource
import cats.implicits._
import co.topl.algebras.ToplGenusRpc
import co.topl.genus.services._
import co.topl.genusLibrary.algebras.BlockFetcherAlgebra
import co.topl.genusLibrary.algebras.TransactionFetcherAlgebra
import fs2.grpc.syntax.all._
import io.grpc.{Metadata, Server}
import io.grpc.netty.shaded.io.grpc.netty.{NettyChannelBuilder, NettyServerBuilder}
import io.grpc.protobuf.services.ProtoReflectionService
import java.net.InetSocketAddress

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

    def serve[F[_]: Async](
      host:                      String,
      port:                      Int,
      blockFetcher:              BlockFetcherAlgebra[F],
      transactionFetcherAlgebra: TransactionFetcherAlgebra[F]
    ): Resource[F, Server] =
      for {
        fullBlockService <- BlockServiceFs2Grpc.bindServiceResource(
          new GrpcBlockService(blockFetcher)
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
