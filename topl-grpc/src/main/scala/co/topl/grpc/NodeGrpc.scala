package co.topl.grpc

import cats.Eval
import cats.MonadThrow
import cats.Now
import cats.effect.kernel.Async
import cats.effect.kernel.Resource
import cats.implicits._
import co.topl.algebras.SynchronizationTraversalStep
import co.topl.algebras.SynchronizationTraversalSteps
import co.topl.algebras.NodeRpc
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.consensus.models._
import co.topl.models.Epoch
import co.topl.node.models.BlockBody
import co.topl.node.services._
import co.topl.proto.node.{EpochData, NodeConfig}
import fs2.Stream
import fs2.grpc.syntax.all._
import io.grpc.Metadata
import io.grpc.Server
import io.grpc.ServerServiceDefinition
import io.grpc.netty.shaded.io.grpc.netty.NettyChannelBuilder
import io.grpc.netty.shaded.io.grpc.netty.NettyServerBuilder
import io.grpc.protobuf.services.ProtoReflectionService
import java.net.InetSocketAddress

object NodeGrpc {

  object Client {

    /**
     * Creates a Topl RPC Client for interacting with a Bifrost node
     * @param host Bifrost node host/IP
     * @param port Bifrost node port
     * @param tls Should the connection use TLS?
     */
    def make[F[_]: Async](host: String, port: Int, tls: Boolean): Resource[F, NodeRpc[F, Stream[F, *]]] =
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
          new NodeRpc[F, Stream[F, *]] {

            def broadcastTransaction(transaction: IoTransaction): F[Unit] =
              client
                .broadcastTransaction(
                  BroadcastTransactionReq(transaction),
                  new Metadata()
                )
                .void

            def currentMempool(): F[Set[TransactionId]] =
              client
                .currentMempool(
                  CurrentMempoolReq(),
                  new Metadata()
                )
                .map(_.transactionIds.toSet)

            override def currentMempoolContains(transactionId: TransactionId): F[Boolean] =
              client
                .currentMempoolContains(
                  CurrentMempoolContainsReq(transactionId),
                  new Metadata()
                )
                .map(_.inMempool)

            def fetchBlockHeader(blockId: BlockId): F[Option[BlockHeader]] =
              client
                .fetchBlockHeader(
                  FetchBlockHeaderReq(blockId),
                  new Metadata()
                )
                .map(_.header)

            def fetchBlockBody(blockId: BlockId): F[Option[BlockBody]] =
              client
                .fetchBlockBody(
                  FetchBlockBodyReq(blockId),
                  new Metadata()
                )
                .map(_.body)

            def fetchTransaction(transactionId: TransactionId): F[Option[IoTransaction]] =
              client
                .fetchTransaction(FetchTransactionReq(transactionId), new Metadata())
                .map(_.transaction)

            def blockIdAtHeight(height: Long): F[Option[BlockId]] =
              client
                .fetchBlockIdAtHeight(FetchBlockIdAtHeightReq(height), new Metadata())
                .map(_.blockId)

            def blockIdAtDepth(depth: Long): F[Option[BlockId]] =
              client
                .fetchBlockIdAtDepth(FetchBlockIdAtDepthReq(depth), new Metadata())
                .map(_.blockId)

            def synchronizationTraversal(): F[Stream[F, SynchronizationTraversalStep]] =
              Async[F].delay {
                client
                  .synchronizationTraversal(
                    SynchronizationTraversalReq(),
                    new Metadata()
                  )
                  .map(_.status match {
                    case SynchronizationTraversalRes.Status.Applied(value) =>
                      SynchronizationTraversalSteps.Applied(value)
                    case SynchronizationTraversalRes.Status.Unapplied(value) =>
                      SynchronizationTraversalSteps.Unapplied(value)
                    case e =>
                      throw new MatchError(e)
                  })
              }

            def fetchProtocolConfigs(): F[Stream[F, NodeConfig]] =
              Async[F].delay(
                client
                  .fetchNodeConfig(FetchNodeConfigReq(), new Metadata())
                  .map(_.config)
              )

            def fetchEpochData(epoch: Option[Epoch]): F[Option[EpochData]] =
              client
                .fetchEpochData(FetchEpochDataReq(epoch), new Metadata())
                .map(_.epochData)

          }
        )
  }

  object Server {

    /**
     * Serves the given gRPC Services
     * @param host The host to bind
     * @param port The port to bind
     * @param services The gRPC services to launch
     */
    def serve[F[_]: Async](host: String, port: Int)(services: List[ServerServiceDefinition]): Resource[F, Server] =
      services
        .foldLeft(
          NettyServerBuilder
            .forAddress(new InetSocketAddress(host, port))
        )(_.addService(_))
        .addService(ProtoReflectionService.newInstance())
        .resource[F]
        .evalMap(server => Async[F].delay(server.start()))

    /**
     * Constructs a gRPC ServerServiceDefinition
     * @param interpreter a base ToplRpc interpreter
     */
    def service[F[_]: Async](interpreter: NodeRpc[F, Stream[F, *]]): Resource[F, ServerServiceDefinition] =
      NodeRpcFs2Grpc.bindServiceResource(new GrpcServerImpl[F](interpreter))

    class GrpcServerImpl[F[_]: MonadThrow](interpreter: NodeRpc[F, Stream[F, *]]) extends NodeRpcFs2Grpc[F, Metadata] {

      def broadcastTransaction(in: BroadcastTransactionReq, ctx: Metadata): F[BroadcastTransactionRes] =
        interpreter
          .broadcastTransaction(in.transaction)
          .as(BroadcastTransactionRes())
          .adaptErrorsToGrpc

      def currentMempool(in: CurrentMempoolReq, ctx: Metadata): F[CurrentMempoolRes] =
        interpreter
          .currentMempool()
          .map(_.toList)
          .map(CurrentMempoolRes(_))
          .adaptErrorsToGrpc

      override def currentMempoolContains(
        request: CurrentMempoolContainsReq,
        ctx:     Metadata
      ): F[CurrentMempoolContainsRes] =
        interpreter
          .currentMempoolContains(request.transactionId)
          .map(CurrentMempoolContainsRes(_))
          .adaptErrorsToGrpc

      def fetchBlockHeader(in: FetchBlockHeaderReq, ctx: Metadata): F[FetchBlockHeaderRes] =
        interpreter
          .fetchBlockHeader(in.blockId)
          .map(FetchBlockHeaderRes(_))
          .adaptErrorsToGrpc

      def fetchBlockBody(in: FetchBlockBodyReq, ctx: Metadata): F[FetchBlockBodyRes] =
        interpreter
          .fetchBlockBody(in.blockId)
          .map(FetchBlockBodyRes(_))
          .adaptErrorsToGrpc

      def fetchTransaction(in: FetchTransactionReq, ctx: Metadata): F[FetchTransactionRes] =
        interpreter
          .fetchTransaction(in.transactionId)
          .map(FetchTransactionRes(_))
          .adaptErrorsToGrpc

      def fetchBlockIdAtHeight(in: FetchBlockIdAtHeightReq, ctx: Metadata): F[FetchBlockIdAtHeightRes] =
        interpreter
          .blockIdAtHeight(in.height)
          .map(FetchBlockIdAtHeightRes(_))
          .adaptErrorsToGrpc

      def fetchBlockIdAtDepth(in: FetchBlockIdAtDepthReq, ctx: Metadata): F[FetchBlockIdAtDepthRes] =
        interpreter
          .blockIdAtDepth(in.depth)
          .map(FetchBlockIdAtDepthRes(_))
          .adaptErrorsToGrpc

      def synchronizationTraversal(
        in:  SynchronizationTraversalReq,
        ctx: Metadata
      ): Stream[F, SynchronizationTraversalRes] =
        Stream
          .force(interpreter.synchronizationTraversal())
          .map {
            case SynchronizationTraversalSteps.Applied(blockId) =>
              SynchronizationTraversalRes(SynchronizationTraversalRes.Status.Applied(blockId))
            case SynchronizationTraversalSteps.Unapplied(blockId) =>
              SynchronizationTraversalRes(SynchronizationTraversalRes.Status.Unapplied(blockId))
          }

      def fetchNodeConfig(request: FetchNodeConfigReq, ctx: Metadata): Stream[F, FetchNodeConfigRes] =
        Stream
          .force(interpreter.fetchProtocolConfigs())
          .map(FetchNodeConfigRes(_))

      def fetchEpochData(request: FetchEpochDataReq, ctx: Metadata): F[FetchEpochDataRes] =
        interpreter.fetchEpochData(request.epoch).map(FetchEpochDataRes(_))
    }
  }
}
