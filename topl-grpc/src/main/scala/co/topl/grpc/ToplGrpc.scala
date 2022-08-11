package co.topl.grpc

import akka.actor.ClassicActorSystemProvider
import akka.grpc.{GrpcClientSettings, GrpcServiceException}
import akka.http.scaladsl.Http
import cats.MonadThrow
import cats.data.{EitherT, OptionT}
import cats.effect.kernel.{Async, Resource}
import cats.implicits._
import co.topl.algebras.ToplRpc
import co.topl.catsakka._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.grpc.services._
import co.topl.{models => bifrostModels}
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
          def broadcastTransaction(transaction: bifrostModels.Transaction): F[Unit] =
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

          def currentMempool(): F[Set[bifrostModels.TypedIdentifier]] =
            Async[F]
              .fromFuture(
                Async[F].delay(client.currentMempool(services.CurrentMempoolReq()))
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
              Async[F]
                .fromFuture(
                  EitherT(blockId.toF[F, models.BlockId])
                    .leftMap(new IllegalArgumentException(_))
                    .rethrowT
                    .map(blockId =>
                      client.fetchBlockHeader(
                        services.FetchBlockHeaderReq(blockId.some)
                      )
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
              Async[F]
                .fromFuture(
                  EitherT(blockId.toF[F, models.BlockId])
                    .leftMap(new IllegalArgumentException(_))
                    .rethrowT
                    .map(blockId =>
                      client.fetchBlockBody(
                        services.FetchBlockBodyReq(blockId.some)
                      )
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
          (in.transmittableBytes: bifrostModels.Bytes)
            .decodeTransmitted[bifrostModels.Transaction]
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
            .flatMap(ids =>
              EitherT(
                ids.toList.traverse(_.toF[F, models.TransactionId]).map(_.sequence)
              )
                .leftMap(e => new GrpcServiceException(Status.DATA_LOSS.withDescription(e)))
                .rethrowT
            )
            .map(CurrentMempoolRes(_))
            .adaptErrorsToGrpc
        )

      def fetchBlockHeader(in: FetchBlockHeaderReq): Future[FetchBlockHeaderRes] =
        implicitly[FToFuture[F]].apply(
          in.blockId
            .toRight("Missing blockId")
            .toEitherT[F]
            .flatMapF(_.toF[F, bifrostModels.TypedIdentifier])
            .leftMap(_ => new GrpcServiceException(Status.INVALID_ARGUMENT.withDescription("Invalid Block ID")))
            .rethrowT
            .flatMap(id =>
              OptionT(interpreter.fetchBlockHeader(id))
                .semiflatMap(header =>
                  EitherT(header.toF[F, models.BlockHeader])
                    .leftMap(e => new GrpcServiceException(Status.DATA_LOSS.withDescription(e)))
                    .rethrowT
                )
                .value
                .map(services.FetchBlockHeaderRes(_))
            )
            .adaptErrorsToGrpc
        )

      def fetchBlockBody(in: FetchBlockBodyReq): Future[FetchBlockBodyRes] =
        implicitly[FToFuture[F]].apply(
          in.blockId
            .toRight("Missing blockId")
            .toEitherT[F]
            .flatMapF(_.toF[F, bifrostModels.TypedIdentifier])
            .leftMap(_ => new GrpcServiceException(Status.INVALID_ARGUMENT.withDescription("Invalid Block ID")))
            .rethrowT
            .flatMap(id =>
              OptionT(interpreter.fetchBlockBody(id))
                .semiflatMap(body =>
                  EitherT(body.toF[F, models.BlockBody])
                    .leftMap(e => new GrpcServiceException(Status.DATA_LOSS.withDescription(e)))
                    .rethrowT
                )
                .value
                .map(services.FetchBlockBodyRes(_))
            )
            .adaptErrorsToGrpc
        )
    }
  }
}
