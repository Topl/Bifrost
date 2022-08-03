package co.topl.grpc

import akka.actor.ClassicActorSystemProvider
import akka.grpc.{GrpcClientSettings, GrpcServiceException}
import akka.http.scaladsl.Http
import cats.MonadThrow
import cats.data.{EitherT, OptionT, ValidatedNec}
import cats.effect.kernel.{Async, Resource}
import cats.implicits._
import co.topl.algebras.ToplRpc
import co.topl.catsakka._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.grpc.services.{CurrentMempoolReq, CurrentMempoolRes, FetchBlockHeaderReq, FetchBlockHeaderRes}
import co.topl.models._
import com.google.protobuf.ByteString
import io.grpc.Status

import scala.concurrent.Future

object ToplGrpc {

  object Client {

    /**
     * Creates a Topl RPC Client for interacting with a Bifrost node
     * @param host Bifrost node host/IP
     * @param port Bifrost node port
     * @param tls Should the connection use TLS?
     */
    def make[F[_]: Async](host: String, port: Int, tls: Boolean)(implicit
      systemProvider:           ClassicActorSystemProvider
    ): F[ToplRpc[F]] =
      Async[F].delay {
        val client = services.ToplGrpcClient(GrpcClientSettings.connectToServiceAt(host, port).withTls(tls))
        new ToplRpc[F] {
          def broadcastTransaction(transaction: Transaction): F[Unit] =
            Async[F]
              .fromFuture(
                Async[F].delay(
                  client.broadcastTransaction(
                    // Note: All other cases use .transmittedBytes, but for now, keep this as immutableBytes
                    services.BroadcastTransactionReq(transaction.immutableBytes)
                  )
                )
              )
              .void

          def currentMempool(): F[Set[TypedIdentifier]] =
            Async[F]
              .fromFuture(
                Async[F].delay(client.currentMempool(services.CurrentMempoolReq()))
              )
              .flatMap(
                _.transactionIds.toList
                  .traverse[ValidatedNec[String, *], TypedIdentifier](data =>
                    (data: Bytes).decodeTransmitted[TypedIdentifier].toValidatedNec
                  )
                  .map(_.toSet)
                  .leftMap(errors => new IllegalArgumentException(show"Invalid Transaction bytes. reason=$errors"))
                  .liftTo[F]
              )

          def fetchBlockHeader(blockId: TypedIdentifier): F[Option[BlockHeaderV2]] =
            OptionT(
              Async[F]
                .fromFuture(
                  Async[F].delay(
                    client.fetchBlockHeader(
                      services.FetchBlockHeaderReq(blockId.transmittableBytes)
                    )
                  )
                )
                .map(_.header)
            )
              .semiflatMap(protoHeader =>
                EitherT(protoHeader.pure[F].toA[BlockHeaderV2])
                  .leftMap(new IllegalArgumentException(_))
                  .rethrowT
              )
              .value
        }
      }
  }

  object Server {

    /**
     * Serves the given ToplRpc interpreter over gRPC
     * @param host The host to bind
     * @param port The port to bind
     * @param interpreter The interpreter which fulfills the data requests
     */
    def serve[F[_]: Async: FToFuture](host: String, port: Int, interpreter: ToplRpc[F])(implicit
      systemProvider:                       ClassicActorSystemProvider
    ): Resource[F, Http.ServerBinding] =
      Resource.make(
        Async[F].fromFuture(
          Async[F].delay(
            Http()
              .newServerAt(host, port)
              .bind(services.ToplGrpcHandler(new GrpcServerImpl[F](interpreter)))
          )
        )
      )(binding => Async[F].fromFuture(Async[F].delay(binding.unbind())).void)

    private[grpc] class GrpcServerImpl[F[_]: MonadThrow: FToFuture](interpreter: ToplRpc[F]) extends services.ToplGrpc {

      def broadcastTransaction(in: services.BroadcastTransactionReq): Future[services.BroadcastTransactionRes] =
        implicitly[FToFuture[F]].apply(
          (in.transmittableBytes: Bytes)
            .decodeTransmitted[Transaction]
            .leftMap(err =>
              new GrpcServiceException(
                Status.INVALID_ARGUMENT.withDescription(s"Invalid Transaction bytes. reason=$err")
              )
            )
            .liftTo[F]
            .flatMap(interpreter.broadcastTransaction)
            .as(services.BroadcastTransactionRes())
            .adaptErrorsToGrpc
        )

      def currentMempool(in: CurrentMempoolReq): Future[CurrentMempoolRes] =
        implicitly[FToFuture[F]].apply(
          interpreter
            .currentMempool()
            .map(ids => CurrentMempoolRes(ids.toList.map(_.transmittableBytes: ByteString)))
            .adaptErrorsToGrpc
        )

      def fetchBlockHeader(in: FetchBlockHeaderReq): Future[FetchBlockHeaderRes] =
        implicitly[FToFuture[F]].apply(
          (in.blockId: Bytes)
            .decodeTransmitted[TypedIdentifier]
            .leftMap(_ => new GrpcServiceException(Status.INVALID_ARGUMENT.withDescription("Invalid Block ID")))
            .liftTo[F]
            .flatMap(id =>
              OptionT(interpreter.fetchBlockHeader(id))
                .semiflatMap(header =>
                  EitherT(header.pure[F].toB[services.BlockHeader])
                    .leftMap(e => new GrpcServiceException(Status.DATA_LOSS.withDescription(e)))
                    .rethrowT
                )
                .value
                .map(services.FetchBlockHeaderRes(_))
                .adaptErrorsToGrpc
            )
        )
    }
  }
}
