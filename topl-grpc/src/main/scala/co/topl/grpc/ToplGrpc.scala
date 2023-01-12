package co.topl.grpc

import cats.{Eval, MonadThrow, Now}
import cats.data.{EitherT, OptionT}
import cats.effect.kernel.{Async, Resource}
import cats.implicits._
import co.topl.algebras.{SynchronizationTraversalStep, ToplRpc}
import co.topl.models.TypedBytes
import com.google.protobuf.ByteString
//import co.topl.proto.services._
import co.topl.node.services._
import co.topl.models.TypedIdentifier
import co.topl.{models => bifrostModels}
import fs2.grpc.syntax.all._
import fs2.Stream
import io.grpc.netty.shaded.io.grpc.netty.{NettyChannelBuilder, NettyServerBuilder}
import io.grpc.{Metadata, Server, Status}
import java.net.InetSocketAddress
import co.topl.proto.models

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

            def broadcastTransaction(transaction: bifrostModels.Transaction): F[Unit] =
              EitherT(transaction.toF[F, models.Transaction])
                .leftMap(new IllegalArgumentException(_))
                .rethrowT
                .flatMap(protoTransaction =>
                  client.broadcastTransaction(
                    BroadcastTransactionReq(protoTransaction.some),
                    new Metadata()
                  )
                )
                .void

            def currentMempool(): F[Set[bifrostModels.TypedIdentifier]] = ???
//            def currentMempool(): F[Set[bifrostModels.TypedIdentifier]] =
//              client
//                .currentMempool(
//                  CurrentMempoolReq(),
//                  new Metadata()
//                )
//                .flatMap(p =>
//                  EitherT(
//                    p.transactionIds.toList
//                      .traverse(_.toF[F, bifrostModels.TypedIdentifier])
//                      .map(_.sequence)
//                  )
//                    .map(_.toSet)
//                    .leftMap(errors => new IllegalArgumentException(show"Invalid Transaction bytes. reason=$errors"))
//                    .rethrowT
//                )

            def fetchBlockHeader(
              blockId: bifrostModels.TypedIdentifier
            ): F[Option[co.topl.consensus.models.BlockHeader]] =
              OptionT(
                EitherT(blockId.dataBytes.toF[F, ByteString])
                  .leftMap(new IllegalArgumentException(_))
                  .rethrowT
                  .flatMap(blockId =>
                    client.fetchBlockHeader(
                      FetchBlockHeaderReq(blockId),
                      new Metadata()
                    )
                  )
                  .map(_.header)
              ).value

            def fetchBlockBody(blockId: bifrostModels.TypedIdentifier): F[Option[bifrostModels.BlockBody]] = ???
//            def fetchBlockBody(blockId: bifrostModels.TypedIdentifier): F[Option[bifrostModels.BlockBody]] =
//              OptionT(
//                EitherT(blockId.toF[F, models.BlockId])
//                  .leftMap(new IllegalArgumentException(_))
//                  .rethrowT
//                  .flatMap(blockId =>
//                    client.fetchBlockBody(
//                      FetchBlockBodyReq(blockId.some),
//                      new Metadata()
//                    )
//                  )
//                  .map(_.body)
//              )
//                .semiflatMap(protoBody =>
//                  EitherT(protoBody.toF[F, bifrostModels.BlockBody])
//                    .leftMap(new IllegalArgumentException(_))
//                    .rethrowT
//                )
//                .value

            def fetchTransaction(transactionId: bifrostModels.TypedIdentifier): F[Option[bifrostModels.Transaction]] =
              ???
//            def fetchTransaction(transactionId: bifrostModels.TypedIdentifier): F[Option[bifrostModels.Transaction]] =
//              OptionT(
//                EitherT(transactionId.toF[F, models.TransactionId])
//                  .leftMap(new IllegalArgumentException(_))
//                  .rethrowT
//                  .flatMap(transactionId =>
//                    client.fetchTransaction(
//                      FetchTransactionReq(transactionId.some),
//                      new Metadata()
//                    )
//                  )
//                  .map(_.transaction)
//              )
//                .semiflatMap(protoTransaction =>
//                  EitherT(protoTransaction.toF[F, bifrostModels.Transaction])
//                    .leftMap(new IllegalArgumentException(_))
//                    .rethrowT
//                )
//                .value

            def blockIdAtHeight(height: Long): F[Option[TypedIdentifier]] =
              client
                .fetchBlockIdAtHeight(
                  FetchBlockIdAtHeightReq(height),
                  new Metadata()
                )
                .map(res => TypedBytes.headerFromProtobufString(res.blockId).some)

            def blockIdAtDepth(depth: Long): F[Option[TypedIdentifier]] =
              client
                .fetchBlockIdAtDepth(FetchBlockIdAtDepthReq(depth), new Metadata())
                .map(res => TypedBytes.headerFromProtobufString(res.blockId).some)

            def synchronizationTraversal(): F[Stream[F, SynchronizationTraversalStep]] = ???
//            def synchronizationTraversal(): F[Stream[F, SynchronizationTraversalStep]] =
//              Async[F].delay {
//                client
//                  .synchronizationTraversal(
//                    SynchronizationTraversalReq(),
//                    new Metadata()
//                  )
//                  .evalMap[F, SynchronizationTraversalStep] { r =>
//                    r.status match {
//
//                      case SynchronizationTraversalRes.Status.Empty =>
//                        EitherT
//                          .fromOptionF(Option.empty[bifrostModels.TypedIdentifier].pure[F], "empty")
//                          .leftMap(new IllegalArgumentException(_))
//                          .rethrowT
//                          // This Applied is not reachable
//                          .map(SynchronizationTraversalSteps.Applied)
//
//                      case SynchronizationTraversalRes.Status.Applied(value) =>
//                        value
//                          .toF[F, bifrostModels.TypedIdentifier]
//                          .flatMap(
//                            EitherT
//                              .fromEither[F](_)
//                              .leftMap(new IllegalArgumentException(_))
//                              .rethrowT
//                          )
//                          .map(SynchronizationTraversalSteps.Applied)
//
//                      case SynchronizationTraversalRes.Status.Unapplied(value) =>
//                        value
//                          .toF[F, bifrostModels.TypedIdentifier]
//                          .flatMap(
//                            EitherT
//                              .fromEither[F](_)
//                              .leftMap(new IllegalArgumentException(_))
//                              .rethrowT
//                          )
//                          .map(SynchronizationTraversalSteps.Unapplied)
//                    }
//                  }
//              }

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

      def broadcastTransaction(in: BroadcastTransactionReq, ctx: Metadata): F[BroadcastTransactionRes] = ???
//      def broadcastTransaction(in: BroadcastTransactionReq, ctx: Metadata): F[BroadcastTransactionRes] =
//        in.transaction
//          .toRight("Missing transaction")
//          .toEitherT[F]
//          .flatMapF(_.toF[F, bifrostModels.Transaction])
//          .leftMap(err =>
//            Status.INVALID_ARGUMENT
//              .withDescription(s"Invalid Transaction bytes. reason=$err")
//              .asException()
//          )
//          .rethrowT
//          .flatMap(interpreter.broadcastTransaction)
//          .as(BroadcastTransactionRes())
//          .adaptErrorsToGrpc

      def currentMempool(in: CurrentMempoolReq, ctx: Metadata): F[CurrentMempoolRes] = ???
//      def currentMempool(in: CurrentMempoolReq, ctx: Metadata): F[CurrentMempoolRes] =
//        interpreter
//          .currentMempool()
//          .flatMap(ids =>
//            EitherT(
//              ids.toList.traverse(_.toF[F, models.TransactionId]).map(_.sequence)
//            )
//              .leftMap(e => Status.DATA_LOSS.withDescription(e).asException())
//              .rethrowT
//          )
//          .map(CurrentMempoolRes(_))
//          .adaptErrorsToGrpc

      def fetchBlockHeader(in: FetchBlockHeaderReq, ctx: Metadata): F[FetchBlockHeaderRes] =
        in.blockId
          .asRight[String]
          .toEitherT[F]
          .flatMapF(_.toF[F, bifrostModels.TypedIdentifier](implicitly, blockIdHeaderIsomorphism[F].baMorphism))
          .leftMap(_ => Status.INVALID_ARGUMENT.withDescription("Invalid Block ID").asException())
          .rethrowT
          .flatMap(id =>
            interpreter
              .fetchBlockHeader(id)
              .map(FetchBlockHeaderRes(_))
          )
          .adaptErrorsToGrpc

      def fetchBlockBody(in: FetchBlockBodyReq, ctx: Metadata): F[FetchBlockBodyRes] = ???
//      def fetchBlockBody(in: FetchBlockBodyReq, ctx: Metadata): F[FetchBlockBodyRes] =
//        in.blockId
//          .toRight("Missing blockId")
//          .toEitherT[F]
//          .flatMapF(_.toF[F, bifrostModels.TypedIdentifier])
//          .leftMap(_ => Status.INVALID_ARGUMENT.withDescription("Invalid Block ID").asException())
//          .rethrowT
//          .flatMap(id =>
//            OptionT(interpreter.fetchBlockBody(id))
//              .semiflatMap(body =>
//                EitherT(body.toF[F, models.BlockBody])
//                  .leftMap(e => Status.DATA_LOSS.withDescription(e).asException())
//                  .rethrowT
//              )
//              .value
//              .map(FetchBlockBodyRes(_))
//          )
//          .adaptErrorsToGrpc

      def fetchTransaction(in: FetchTransactionReq, ctx: Metadata): F[FetchTransactionRes] = ???
//      def fetchTransaction(in: FetchTransactionReq, ctx: Metadata): F[FetchTransactionRes] =
//        in.transactionId
//          .toRight("Missing transactionId")
//          .toEitherT[F]
//          .flatMapF(_.toF[F, bifrostModels.TypedIdentifier])
//          .leftMap(e => Status.INVALID_ARGUMENT.withDescription(e).asException())
//          .rethrowT
//          .flatMap(id =>
//            OptionT(interpreter.fetchTransaction(id))
//              .semiflatMap(transaction =>
//                EitherT(transaction.toF[F, models.Transaction])
//                  .leftMap(e => Status.DATA_LOSS.withDescription(e).asException())
//                  .rethrowT
//              )
//              .value
//              .map(FetchTransactionRes(_))
//          )
//          .adaptErrorsToGrpc

      def fetchBlockIdAtHeight(in: FetchBlockIdAtHeightReq, ctx: Metadata): F[FetchBlockIdAtHeightRes] =
        OptionT(interpreter.blockIdAtHeight(in.height))
          .semiflatMap(id =>
            EitherT(id.toF[F, models.BlockId])
              .leftMap(e => Status.DATA_LOSS.withDescription(e).asException())
              .rethrowT
          )
          .semiflatMap(blockId => FetchBlockIdAtHeightRes(blockId.value).pure[F])
          .getOrRaise(Status.DATA_LOSS.withDescription("blockIdAtHeight not Found").asException())
          .adaptErrorsToGrpc

      def fetchBlockIdAtDepth(in: FetchBlockIdAtDepthReq, ctx: Metadata): F[FetchBlockIdAtDepthRes] =
        OptionT(interpreter.blockIdAtDepth(in.depth))
          .semiflatMap(id =>
            EitherT(id.toF[F, models.BlockId])
              .leftMap(e => Status.DATA_LOSS.withDescription(e).asException())
              .rethrowT
          )
          .semiflatMap(blockId => FetchBlockIdAtDepthRes(blockId.value).pure[F])
          .getOrRaise(Status.DATA_LOSS.withDescription("blockIdAtDepth not Found").asException())
          .adaptErrorsToGrpc

//      private def pipeSteps: Pipe[F, SynchronizationTraversalStep, SynchronizationTraversalRes] = { in =>
//        in.evalMap {
//          case SynchronizationTraversalSteps.Applied(blockId) =>
//            EitherT(blockId.toF[F, models.BlockId])
//              .leftMap(e => Status.DATA_LOSS.withDescription(e).asException())
//              .rethrowT
//              .map(SynchronizationTraversalRes.Status.Applied)
//              .map(SynchronizationTraversalRes(_))
//
//          case SynchronizationTraversalSteps.Unapplied(blockId) =>
//            EitherT(blockId.toF[F, models.BlockId])
//              .leftMap(e => Status.DATA_LOSS.withDescription(e).asException())
//              .rethrowT
//              .map(SynchronizationTraversalRes.Status.Unapplied)
//              .map(SynchronizationTraversalRes(_))
//        }
//      }

      def synchronizationTraversal(
        in:  SynchronizationTraversalReq,
        ctx: Metadata
      ): Stream[F, SynchronizationTraversalRes] = ???
//      def synchronizationTraversal(
//        in:  SynchronizationTraversalReq,
//        ctx: Metadata
//      ): Stream[F, SynchronizationTraversalRes] =
//        Stream
//          .eval(
//            interpreter
//              .synchronizationTraversal()
//              .map(_.through(pipeSteps))
//              .adaptErrorsToGrpc
//          )
//          .flatten
    }
  }
}
