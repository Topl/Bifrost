package co.topl.grpc

import cats.{Eval, MonadThrow, Now}
import cats.data.{EitherT, OptionT}
import cats.effect.kernel.{Async, Resource}
import cats.implicits._
import co.topl.algebras.ToplRpc
import co.topl.grpc.services._
import co.topl.models.TypedIdentifier
import co.topl.{models => bifrostModels}
import fs2.grpc.syntax.all._
import io.grpc.netty.shaded.io.grpc.netty.{NettyChannelBuilder, NettyServerBuilder}
import io.grpc.{Metadata, Server, ServerServiceDefinition, Status}
import java.net.InetSocketAddress

object ToplGrpc {

  object Client {

    /**
     * Creates a Topl RPC Client for interacting with a Bifrost node
     * @param host Bifrost node host/IP
     * @param port Bifrost node port
     * @param tls Should the connection use TLS?
     */
    def make[F[_]: Async](host: String, port: Int, tls: Boolean): F[ToplRpc[F]] = {
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
        .flatMap(ToplGrpcFs2Grpc.stubResource[F])
        .map(client =>
          new ToplRpc[F] {

            def broadcastTransaction(transaction: bifrostModels.Transaction): F[Unit] =
              EitherT(transaction.toF[F, models.Transaction])
                .leftMap(new IllegalArgumentException(_))
                .rethrowT
                .flatMap(protoTransaction =>
                  client.broadcastTransaction(
                    services.BroadcastTransactionReq(protoTransaction.some),
                    new Metadata()
                  )
                )
                .void

            def currentMempool(): F[Set[bifrostModels.TypedIdentifier]] =
              client
                .currentMempool(
                  services.CurrentMempoolReq(),
                  new Metadata()
                )
                .flatMap(p =>
                  EitherT(
                    p.transactionIds.toList
                      .traverse(_.toF[F, bifrostModels.TypedIdentifier])
                      .map(_.sequence)
                  )
                    .map(_.toSet)
                    .leftMap(errors => new IllegalArgumentException(show"Invalid Transaction bytes. reason=$errors"))
                    .rethrowT
                )

            def fetchBlockHeader(blockId: bifrostModels.TypedIdentifier): F[Option[bifrostModels.BlockHeaderV2]] =
              OptionT(
                EitherT(blockId.toF[F, models.BlockId])
                  .leftMap(new IllegalArgumentException(_))
                  .rethrowT
                  .flatMap(blockId =>
                    client.fetchBlockHeader(
                      services.FetchBlockHeaderReq(blockId.some),
                      new Metadata()
                    )
                  )
                  .map(_.header)
              )
                .semiflatMap(protoHeader =>
                  EitherT(protoHeader.toF[F, bifrostModels.BlockHeaderV2])
                    .leftMap(new IllegalArgumentException(_))
                    .rethrowT
                )
                .value

            def fetchBlockBody(blockId: bifrostModels.TypedIdentifier): F[Option[bifrostModels.BlockBodyV2]] =
              OptionT(
                EitherT(blockId.toF[F, models.BlockId])
                  .leftMap(new IllegalArgumentException(_))
                  .rethrowT
                  .flatMap(blockId =>
                    client.fetchBlockBody(
                      services.FetchBlockBodyReq(blockId.some),
                      new Metadata()
                    )
                  )
                  .map(_.body)
              )
                .semiflatMap(protoBody =>
                  EitherT(protoBody.toF[F, bifrostModels.BlockBodyV2])
                    .leftMap(new IllegalArgumentException(_))
                    .rethrowT
                )
                .value

            def fetchTransaction(transactionId: bifrostModels.TypedIdentifier): F[Option[bifrostModels.Transaction]] =
              OptionT(
                EitherT(transactionId.toF[F, models.TransactionId])
                  .leftMap(new IllegalArgumentException(_))
                  .rethrowT
                  .flatMap(transactionId =>
                    client.fetchTransaction(
                      services.FetchTransactionReq(transactionId.some),
                      new Metadata()
                    )
                  )
                  .map(_.transaction)
              )
                .semiflatMap(protoTransaction =>
                  EitherT(protoTransaction.toF[F, bifrostModels.Transaction])
                    .leftMap(new IllegalArgumentException(_))
                    .rethrowT
                )
                .value

            def blockIdAtHeight(height: Long): F[Option[TypedIdentifier]] =
              OptionT(
                client
                  .fetchBlockIdAtHeight(
                    services.FetchBlockIdAtHeightReq(height),
                    new Metadata()
                  )
                  .map(_.blockId)
              )
                .semiflatMap(_.toF[F, bifrostModels.TypedIdentifier])
                .semiflatMap(EitherT.fromEither[F](_).leftMap(new IllegalArgumentException(_)).rethrowT)
                .value

            def blockIdAtDepth(depth: Long): F[Option[TypedIdentifier]] =
              OptionT(
                client
                  .fetchBlockIdAtDepth(services.FetchBlockIdAtDepthReq(depth), new Metadata())
                  .map(_.blockId)
              )
                .semiflatMap(_.toF[F, bifrostModels.TypedIdentifier])
                .semiflatMap(EitherT.fromEither[F](_).leftMap(new IllegalArgumentException(_)).rethrowT)
                .value
          }
        )
        .use(client => Async[F].delay(client))
    }
  }

  object Server {

    /**
     * Serves the given ToplRpc interpreter over gRPC
     * @param host The host to bind
     * @param port The port to bind
     * @param interpreter The interpreter which fulfills the data requests
     */
    def serve[F[_]: Async](host: String, port: Int, interpreter: ToplRpc[F]): Resource[F, Server] = {

      def run(serverServiceDefinition: ServerServiceDefinition): F[Server] =
        NettyServerBuilder
          .forAddress(new InetSocketAddress(host, port))
          .addService(serverServiceDefinition)
          .resource[F]
          .map(_.start())
          .use(s => Async[F].delay(s))

      Resource
        .make {
          ToplGrpcFs2Grpc
            .bindServiceResource(
              new GrpcServerImpl(interpreter)
            )
            .use(run)
        }(server => Async[F].delay(server.shutdown().awaitTermination()))
    }

    private[grpc] class GrpcServerImpl[F[_]: MonadThrow](interpreter: ToplRpc[F])
        extends services.ToplGrpcFs2Grpc[F, Metadata] {

      def broadcastTransaction(in: BroadcastTransactionReq, ctx: Metadata): F[BroadcastTransactionRes] =
        in.transaction
          .toRight("Missing transaction")
          .toEitherT[F]
          .flatMapF(_.toF[F, bifrostModels.Transaction])
          .leftMap(err =>
            Status.INVALID_ARGUMENT
              .withDescription(s"Invalid Transaction bytes. reason=$err")
              .asException()
          )
          .rethrowT
          .map(interpreter.broadcastTransaction)
          .as(services.BroadcastTransactionRes())
          .adaptErrorsToGrpc

      def currentMempool(in: CurrentMempoolReq, ctx: Metadata): F[CurrentMempoolRes] =
        interpreter
          .currentMempool()
          .flatMap(ids =>
            EitherT(
              ids.toList.traverse(_.toF[F, models.TransactionId]).map(_.sequence)
            )
              .leftMap(e => Status.DATA_LOSS.withDescription(e).asException())
              .rethrowT
          )
          .map(CurrentMempoolRes(_))
          .adaptErrorsToGrpc

      def fetchBlockHeader(in: FetchBlockHeaderReq, ctx: Metadata): F[FetchBlockHeaderRes] =
        in.blockId
          .toRight("Missing blockId")
          .toEitherT[F]
          .flatMapF(_.toF[F, bifrostModels.TypedIdentifier])
          .leftMap(_ => Status.INVALID_ARGUMENT.withDescription("Invalid Block ID").asException())
          .rethrowT
          .flatMap(id =>
            OptionT(interpreter.fetchBlockHeader(id))
              .semiflatMap(header =>
                EitherT(header.toF[F, models.BlockHeader])
                  .leftMap(e => Status.DATA_LOSS.withDescription(e).asException())
                  .rethrowT
              )
              .value
              .map(services.FetchBlockHeaderRes(_))
          )
          .adaptErrorsToGrpc

      def fetchBlockBody(in: FetchBlockBodyReq, ctx: Metadata): F[FetchBlockBodyRes] =
        in.blockId
          .toRight("Missing blockId")
          .toEitherT[F]
          .flatMapF(_.toF[F, bifrostModels.TypedIdentifier])
          .leftMap(_ => Status.INVALID_ARGUMENT.withDescription("Invalid Block ID").asException())
          .rethrowT
          .flatMap(id =>
            OptionT(interpreter.fetchBlockBody(id))
              .semiflatMap(body =>
                EitherT(body.toF[F, models.BlockBody])
                  .leftMap(e => Status.DATA_LOSS.withDescription(e).asException())
                  .rethrowT
              )
              .value
              .map(services.FetchBlockBodyRes(_))
          )
          .adaptErrorsToGrpc

      def fetchTransaction(in: FetchTransactionReq, ctx: Metadata): F[FetchTransactionRes] =
        in.transactionId
          .toRight("Missing transactionId")
          .toEitherT[F]
          .flatMapF(_.toF[F, bifrostModels.TypedIdentifier])
          .leftMap(e => Status.INVALID_ARGUMENT.withDescription(e).asException())
          .rethrowT
          .flatMap(id =>
            OptionT(interpreter.fetchTransaction(id))
              .semiflatMap(transaction =>
                EitherT(transaction.toF[F, models.Transaction])
                  .leftMap(e => Status.DATA_LOSS.withDescription(e).asException())
                  .rethrowT
              )
              .value
              .map(services.FetchTransactionRes(_))
          )
          .adaptErrorsToGrpc

      def fetchBlockIdAtHeight(in: FetchBlockIdAtHeightReq, ctx: Metadata): F[FetchBlockIdAtHeightRes] =
        OptionT(interpreter.blockIdAtHeight(in.height))
          .semiflatMap(id =>
            EitherT(id.toF[F, models.BlockId])
              .leftMap(e => Status.DATA_LOSS.withDescription(e).asException())
              .rethrowT
          )
          .value
          .map(services.FetchBlockIdAtHeightRes(_))
          .adaptErrorsToGrpc

      def fetchBlockIdAtDepth(in: FetchBlockIdAtDepthReq, ctx: Metadata): F[FetchBlockIdAtDepthRes] =
        OptionT(interpreter.blockIdAtDepth(in.depth))
          .semiflatMap(id =>
            EitherT(id.toF[F, models.BlockId])
              .leftMap(e => Status.DATA_LOSS.withDescription(e).asException())
              .rethrowT
          )
          .value
          .map(services.FetchBlockIdAtDepthRes(_))
          .adaptErrorsToGrpc
    }
  }
}
