package co.topl.grpc

import akka.actor.ClassicActorSystemProvider
import akka.grpc.{GrpcClientSettings, GrpcServiceException}
import akka.http.scaladsl.Http
import cats.MonadThrow
import cats.data.{OptionT, ValidatedNec}
import cats.effect.kernel.{Async, Resource}
import cats.implicits._
import co.topl.algebras.ToplRpc
import co.topl.catsakka._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.grpc.services.{CurrentMempoolReq, CurrentMempoolRes, FetchBlockHeaderReq, FetchBlockHeaderRes}
import co.topl.models._
import co.topl.models.utility.HasLength.instances.{bytesLength, latin1DataLength}
import co.topl.models.utility.StringDataTypes.Latin1Data
import co.topl.models.utility.{Lengths, Sized}
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
                      services.FetchBlockHeaderReq(blockId.allBytes)
                    )
                  )
                )
                .map(_.header)
            )
              .semiflatMap(protoHeader =>
                (for {
                  txRoot <- Sized
                    .strict[Bytes, Lengths.`32`.type](protoHeader.txRoot: Bytes)
                    .leftMap(_ => new IllegalArgumentException("Invalid txRoot"))
                  bloomFilter <- Sized
                    .strict[Bytes, Lengths.`256`.type](protoHeader.bloomFilter: Bytes)
                    .leftMap(_ => new IllegalArgumentException("Invalid bloomFilter"))
                  eligibilityCertificate <- (protoHeader.eligibilityCertificate: Bytes)
                    .decodeImmutable[EligibilityCertificate]
                    .leftMap(_ => new IllegalArgumentException("Invalid eligibilityCertificate"))
                  operationalCertificate <- (protoHeader.operationalCertificate: Bytes)
                    .decodeImmutable[OperationalCertificate]
                    .leftMap(_ => new IllegalArgumentException("Invalid operationalCertificate"))
                  metadata <-
                    if (protoHeader.metadata.isEmpty) Right(None)
                    else
                      Sized
                        .max[Latin1Data, Lengths.`32`.type](Latin1Data.fromData(protoHeader.metadata.toByteArray))
                        .map(_.some)
                        .leftMap(_ => new IllegalArgumentException("Invalid metadata"))
                  address <- (protoHeader.address: Bytes)
                    .decodeImmutable[StakingAddresses.Operator]
                    .leftMap(_ => new IllegalArgumentException("Invalid address"))
                } yield BlockHeaderV2(
                  TypedBytes(protoHeader.parentHeaderId),
                  protoHeader.parentSlot,
                  txRoot,
                  bloomFilter,
                  protoHeader.timestamp,
                  protoHeader.height,
                  protoHeader.slot,
                  eligibilityCertificate,
                  operationalCertificate,
                  metadata,
                  address
                )).liftTo[F]
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
        )

      def currentMempool(in: CurrentMempoolReq): Future[CurrentMempoolRes] =
        implicitly[FToFuture[F]].apply(
          interpreter
            .currentMempool()
            .map(ids => CurrentMempoolRes(ids.toList.map(_.transmittableBytes: ByteString)))
        )

      def fetchBlockHeader(in: FetchBlockHeaderReq): Future[FetchBlockHeaderRes] =
        implicitly[FToFuture[F]].apply(
          (in.blockId: Bytes)
            .decodeImmutable[TypedIdentifier]
            .leftMap(_ => new GrpcServiceException(Status.INVALID_ARGUMENT.withDescription("Invalid Block ID")))
            .liftTo[F]
            .flatMap(id =>
              OptionT(interpreter.fetchBlockHeader(id))
                .map(header =>
                  services.BlockHeader(
                    header.parentHeaderId.immutableBytes,
                    header.parentSlot,
                    header.txRoot.data,
                    header.bloomFilter.data,
                    header.timestamp,
                    header.height,
                    header.slot,
                    header.eligibilityCertificate.immutableBytes,
                    header.operationalCertificate.immutableBytes,
                    header.metadata.fold(Bytes.empty)(v => Bytes(v.data.bytes)),
                    header.address.immutableBytes
                  )
                )
                .value
                .map(services.FetchBlockHeaderRes(_))
            )
        )
    }
  }
}
