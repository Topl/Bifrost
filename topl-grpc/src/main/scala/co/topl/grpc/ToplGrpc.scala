package co.topl.grpc

import cats.data.{EitherT, OptionT}
import cats.effect.kernel.{Async, Resource}
import cats.implicits._
import cats.{Eval, MonadThrow, Now}
import co.topl.algebras.{SynchronizationTraversalStep, SynchronizationTraversalSteps, ToplRpc}
import co.topl.brambl.models.Identifier.IoTransaction32
import co.topl.consensus.models._
import co.topl.models.{TypedBytes, TypedIdentifier}
import co.topl.models.utility._
import co.topl.node.models.BlockBody
import co.topl.node.services._
import co.topl.proto.models.Transaction
import fs2.{Pipe, Stream}
import fs2.grpc.syntax.all._
import io.grpc.netty.shaded.io.grpc.netty.{NettyChannelBuilder, NettyServerBuilder}
import io.grpc.{Metadata, Server, Status}
import java.net.InetSocketAddress

object ToplGrpc {

  object Client {

    /**
     * Creates a Topl RPC Client for interacting with a Bifrost node
     * @param host Bifrost node host/IP
     * @param port Bifrost node port
     * @param tls Should the connection use TLS?
     */
    def make[F[_]: Async](host: String, port: Int, tls: Boolean): Resource[F, ToplRpc[F, Stream[F, *]]] = {
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
        .flatMap(NodeRpcFs2Grpc.stubResource[F])
        .map(client =>
          new ToplRpc[F, Stream[F, *]] {

            def broadcastTransaction(transaction: co.topl.proto.models.Transaction): F[Unit] =
              client
                .broadcastTransaction(
                  BroadcastTransactionReq(transaction.some),
                  new Metadata()
                )
                .void

            def currentMempool(): F[Set[TypedIdentifier]] =
              client
                .currentMempool(
                  CurrentMempoolReq(),
                  new Metadata()
                )
                .flatMap(p =>
                  EitherT(
                    p.transactionIds.toList
                      .traverse(_.toF[F, TypedIdentifier])
                      .map(_.sequence)
                  )
                    .map(_.toSet)
                    .leftMap(errors => new IllegalArgumentException(show"Invalid Transaction bytes. reason=$errors"))
                    .rethrowT
                )

            def fetchBlockHeader(
              blockId: TypedIdentifier
            ): F[Option[BlockHeader]] =
              OptionT(
                EitherT(blockId.toF[F, BlockId])
                  .leftMap(new IllegalArgumentException(_))
                  .rethrowT
                  .flatMap(blockId =>
                    client.fetchBlockHeader(
                      FetchBlockHeaderReq(blockId.some),
                      new Metadata()
                    )
                  )
                  .map(_.header)
              ).value

            def fetchBlockBody(blockId: TypedIdentifier): F[Option[BlockBody]] =
              OptionT(
                EitherT(blockId.toF[F, BlockId])
                  .leftMap(new IllegalArgumentException(_))
                  .rethrowT
                  .flatMap(blockId =>
                    client.fetchBlockBody(
                      FetchBlockBodyReq(blockId.some),
                      new Metadata()
                    )
                  )
                  .map(_.body)
              ).value

            def fetchTransaction(
              transactionId: TypedIdentifier
            ): F[Option[Transaction]] =
              EitherT(transactionId.toF[F, IoTransaction32])
                .leftMap(new IllegalArgumentException(_))
                .rethrowT
                .flatMap(transactionId =>
                  client.fetchTransaction(FetchTransactionReq(transactionId.some), new Metadata())
                )
                .map(_.transaction)

            def blockIdAtHeight(height: Long): F[Option[TypedIdentifier]] =
              OptionT(
                client
                  .fetchBlockIdAtHeight(FetchBlockIdAtHeightReq(height), new Metadata())
                  .map(res => TypedBytes.headerFromBlockId(res.blockId).some)
              ).value

            def blockIdAtDepth(depth: Long): F[Option[TypedIdentifier]] =
              OptionT(
                client
                  .fetchBlockIdAtDepth(FetchBlockIdAtDepthReq(depth), new Metadata())
                  .map(res => TypedBytes.headerFromBlockId(res.blockId).some)
              ).value

            def synchronizationTraversal(): F[Stream[F, SynchronizationTraversalStep]] =
              Async[F].delay {
                client
                  .synchronizationTraversal(
                    SynchronizationTraversalReq(),
                    new Metadata()
                  )
                  .evalMap[F, SynchronizationTraversalStep] { r =>
                    r.status match {

                      case SynchronizationTraversalRes.Status.Empty =>
                        EitherT
                          .fromOptionF(Option.empty[TypedIdentifier].pure[F], "empty")
                          .leftMap(new IllegalArgumentException(_))
                          .rethrowT
                          // This Applied is not reachable
                          .map(SynchronizationTraversalSteps.Applied)

                      case SynchronizationTraversalRes.Status.Applied(value) =>
                        value
                          .toF[F, TypedIdentifier](implicitly, blockIdHeaderIsomorphism[F].baMorphism)
                          .flatMap(
                            EitherT
                              .fromEither[F](_)
                              .leftMap(new IllegalArgumentException(_))
                              .rethrowT
                          )
                          .map(SynchronizationTraversalSteps.Applied)

                      case SynchronizationTraversalRes.Status.Unapplied(value) =>
                        value
                          .toF[F, TypedIdentifier](implicitly, blockIdHeaderIsomorphism[F].baMorphism)
                          .flatMap(
                            EitherT
                              .fromEither[F](_)
                              .leftMap(new IllegalArgumentException(_))
                              .rethrowT
                          )
                          .map(SynchronizationTraversalSteps.Unapplied)
                    }
                  }
              }

          }
        )
    }
  }

  object Server {

    /**
     * Serves the given ToplRpc interpreter over gRPC
     * @param host The host to bind
     * @param port The port to bind
     * @param interpreter The interpreter which fulfills the data requests
     */
    def serve[F[_]: Async, S[_]](host: String, port: Int, interpreter: ToplRpc[F, Stream[F, *]]): Resource[F, Server] =
      NodeRpcFs2Grpc
        .bindServiceResource(
          new GrpcServerImpl(interpreter)
        )
        .flatMap(serverServiceDefinition =>
          NettyServerBuilder
            .forAddress(new InetSocketAddress(host, port))
            .addService(serverServiceDefinition)
            .resource[F]
            .evalMap(server => Async[F].delay(server.start()))
        )

    private[grpc] class GrpcServerImpl[F[_]: MonadThrow](interpreter: ToplRpc[F, Stream[F, *]])
        extends NodeRpcFs2Grpc[F, Metadata] {

      def broadcastTransaction(in: BroadcastTransactionReq, ctx: Metadata): F[BroadcastTransactionRes] =
        in.transaction
          .toRight("Missing transaction")
          .toEitherT[F]
          .semiflatMap(interpreter.broadcastTransaction)
          .leftMap(new IllegalArgumentException(_))
          .as(BroadcastTransactionRes())
          .rethrowT
          .adaptErrorsToGrpc

      def currentMempool(in: CurrentMempoolReq, ctx: Metadata): F[CurrentMempoolRes] =
        interpreter
          .currentMempool()
          .flatMap(ids =>
            EitherT(
              ids.toList.traverse(_.toF[F, IoTransaction32]).map(_.sequence)
            )
              .leftMap(e => Status.DATA_LOSS.withDescription(e).asException())
              .rethrowT
          )
          .map(CurrentMempoolRes(_))
          .adaptErrorsToGrpc

      def fetchBlockHeader(in: FetchBlockHeaderReq, ctx: Metadata): F[FetchBlockHeaderRes] =
        in.blockId
          .toRight("Missing Block ID")
          .toEitherT[F]
          .flatMapF(_.toF[F, TypedIdentifier])
          .leftMap(_ => Status.INVALID_ARGUMENT.withDescription("Invalid Block ID").asException())
          .rethrowT
          .flatMap(id =>
            interpreter
              .fetchBlockHeader(id)
              .map(FetchBlockHeaderRes(_))
          )
          .adaptErrorsToGrpc

      def fetchBlockBody(in: FetchBlockBodyReq, ctx: Metadata): F[FetchBlockBodyRes] =
        in.blockId
          .toRight("Missing Block ID")
          .toEitherT[F]
          .flatMapF(_.toF[F, TypedIdentifier])
          .leftMap(_ => Status.INVALID_ARGUMENT.withDescription("Invalid Block ID").asException())
          .rethrowT
          .flatMap(id =>
            OptionT(interpreter.fetchBlockBody(id)).value
              .map(FetchBlockBodyRes(_))
          )
          .adaptErrorsToGrpc

      /**
       * TODO: Replace with Brambl's IoTransaction
       * @see  https://github.com/Topl/protobuf-specs/blob/main/node/services/bifrost_rpc.proto#L89
       * @param in
       * @param ctx
       * @return
       */
      def fetchTransaction(in: FetchTransactionReq, ctx: Metadata): F[FetchTransactionRes] =
        in.transactionId
          .toRight("Missing transactionId")
          .toEitherT[F]
          .flatMapF(_.toF[F, TypedIdentifier])
          .leftMap(e => Status.INVALID_ARGUMENT.withDescription(e).asException())
          .rethrowT
          .flatMap(interpreter.fetchTransaction)
          .map(FetchTransactionRes(_))
          .adaptErrorsToGrpc

      def fetchBlockIdAtHeight(in: FetchBlockIdAtHeightReq, ctx: Metadata): F[FetchBlockIdAtHeightRes] =
        OptionT(interpreter.blockIdAtHeight(in.height))
          .semiflatMap(id =>
            EitherT(id.toF[F, BlockId])
              .leftMap(e => Status.DATA_LOSS.withDescription(e).asException())
              .rethrowT
          )
          .semiflatMap(blockId => FetchBlockIdAtHeightRes(blockId.some).pure[F])
          .getOrRaise(Status.DATA_LOSS.withDescription("blockIdAtHeight not Found").asException())
          .adaptErrorsToGrpc

      def fetchBlockIdAtDepth(in: FetchBlockIdAtDepthReq, ctx: Metadata): F[FetchBlockIdAtDepthRes] =
        OptionT(interpreter.blockIdAtDepth(in.depth))
          .semiflatMap(id =>
            EitherT(id.toF[F, BlockId])
              .leftMap(e => Status.DATA_LOSS.withDescription(e).asException())
              .rethrowT
          )
          .semiflatMap(blockId => FetchBlockIdAtDepthRes(blockId.some).pure[F])
          .getOrRaise(Status.DATA_LOSS.withDescription("blockIdAtDepth not Found").asException())
          .adaptErrorsToGrpc

      private def pipeSteps: Pipe[F, SynchronizationTraversalStep, SynchronizationTraversalRes] = { in =>
        in.evalMap {
          case SynchronizationTraversalSteps.Applied(blockId) =>
            EitherT(blockId.toF[F, BlockId])
              .leftMap(e => Status.DATA_LOSS.withDescription(e).asException())
              .rethrowT
              .map(SynchronizationTraversalRes.Status.Applied)
              .map(SynchronizationTraversalRes(_))

          case SynchronizationTraversalSteps.Unapplied(blockId) =>
            EitherT(blockId.toF[F, BlockId])
              .leftMap(e => Status.DATA_LOSS.withDescription(e).asException())
              .rethrowT
              .map(SynchronizationTraversalRes.Status.Unapplied)
              .map(SynchronizationTraversalRes(_))
        }
      }

      def synchronizationTraversal(
        in:  SynchronizationTraversalReq,
        ctx: Metadata
      ): Stream[F, SynchronizationTraversalRes] =
        Stream
          .eval(
            interpreter
              .synchronizationTraversal()
              .map(_.through(pipeSteps))
              .adaptErrorsToGrpc
          )
          .flatten
    }
  }
}
