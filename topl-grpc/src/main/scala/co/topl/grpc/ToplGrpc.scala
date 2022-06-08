package co.topl.grpc

import akka.NotUsed
import akka.actor.ClassicActorSystemProvider
import akka.grpc.GrpcClientSettings
import akka.http.scaladsl.Http
import akka.stream.scaladsl.Source
import cats.MonadThrow
import cats.data.ValidatedNec
import cats.effect.kernel.{Async, Resource}
import cats.implicits._
import co.topl.algebras.ToplRpc
import co.topl.catsakka._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.models._
import co.topl.models.utility.HasLength.instances.{bytesLength, latin1DataLength}
import co.topl.models.utility.Sized
import co.topl.models.utility.StringDataTypes.Latin1Data
import com.google.protobuf.ByteString
import scodec.bits.ByteVector

import scala.concurrent.Future
import scala.language.implicitConversions

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
    ): F[ToplRpc[F, Source[*, NotUsed]]] =
      Async[F].delay {
        val client = services.ToplGrpcClient(GrpcClientSettings.connectToServiceAt(host, port).withTls(tls))
        new ToplRpc[F, Source[*, NotUsed]] {
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

          def blockAdoptions(): F[Source[TypedIdentifier, NotUsed]] =
            Async[F].delay(
              client
                .blockAdoptions(services.BlockAdoptionsReq())
                .map(_.blockId.asReadOnlyByteBuffer())
                .map(ByteVector(_))
                .map(TypedBytes(_))
            )

          def fetchHeader(id: TypedIdentifier): F[Option[BlockHeaderV2]] =
            Async[F]
              .fromFuture(
                Async[F].delay(
                  client.fetchBlockHeader(
                    services.FetchBlockHeaderReq(id.allBytes)
                  )
                )
              )
              .map(res =>
                res.header.map(h =>
                  BlockHeaderV2(
                    TypedBytes(h.parentHeaderIdBytes),
                    h.parentSlot,
                    Sized.strictUnsafe(h.txRootBytes: Bytes),
                    Sized.strictUnsafe(h.bloomFilterBytes: Bytes),
                    h.timestamp,
                    h.height,
                    h.slot,
                    (h.eligibilityCertificateBytes: Bytes).decodeImmutable[EligibilityCertificate].getOrElse(???),
                    (h.operationalCertificateBytes: Bytes).decodeImmutable[OperationalCertificate].getOrElse(???),
                    h.metadataBytes.some
                      .filter(_.nonEmpty)
                      .map(b => Latin1Data.fromData(b.toByteArray))
                      .map(Sized.maxUnsafe(_)),
                    (h.addressBytes: Bytes).decodeImmutable[StakingAddresses.Operator].getOrElse(???)
                  )
                )
              )
          def currentMempool(): F[Set[TypedIdentifier]] =
            Async[F]
              .fromFuture(
                Async[F].delay(client.currentMempool(services.CurrentMempoolReq()))
              )
              .map(
                _.transactionIds.toList
                  .traverse[ValidatedNec[String, *], TypedIdentifier](data =>
                    (data: Bytes).decodeTransmitted[TypedIdentifier].toValidatedNec
                  )
                  .map(_.toSet)
                  .leftMap(errors => new IllegalArgumentException(show"Invalid Transaction bytes. reason=$errors"))
                  .toEither
              )
              .rethrow
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
    def serve[F[_]: Async: FToFuture](host: String, port: Int, interpreter: ToplRpc[F, Source[*, NotUsed]])(implicit
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

    private[grpc] class GrpcServerImpl[F[_]: MonadThrow: FToFuture](interpreter: ToplRpc[F, Source[*, NotUsed]])
        extends services.ToplGrpc {

      def broadcastTransaction(in: services.BroadcastTransactionReq): Future[services.BroadcastTransactionRes] =
        implicitly[FToFuture[F]].apply(
          (in.transmittableBytes: Bytes)
            .decodeTransmitted[Transaction]
            .leftMap(err => new IllegalArgumentException(s"Invalid Transaction bytes. reason=$err"))
            .liftTo[F]
            .flatMap(interpreter.broadcastTransaction)
            .as(services.BroadcastTransactionRes())
        )

      def currentMempool(in: services.CurrentMempoolReq): Future[services.CurrentMempoolRes] =
        implicitly[FToFuture[F]].apply(
          interpreter
            .currentMempool()
            .map(ids => services.CurrentMempoolRes(ids.toList.map(_.transmittableBytes: ByteString)))
        )

      def blockAdoptions(in: services.BlockAdoptionsReq): Source[services.BlockAdoptionsRes, NotUsed] =
        Source
          .futureSource(implicitly[FToFuture[F]].apply(interpreter.blockAdoptions()))
          .map(_.allBytes.toByteBuffer)
          .map(ByteString.copyFrom)
          .map(services.BlockAdoptionsRes(_))
          .mapMaterializedValue(_ => NotUsed)

      def fetchBlockHeader(in: services.FetchBlockHeaderReq): Future[services.FetchBlockHeaderRes] =
        implicitly[FToFuture[F]].apply(
          interpreter
            .fetchHeader(TypedBytes(in.blockId))
            .map(opt =>
              services.FetchBlockHeaderRes(
                opt.map(h =>
                  services.BlockHeader(
                    h.parentHeaderId.allBytes,
                    h.parentSlot,
                    h.txRoot.data,
                    h.bloomFilter.data,
                    h.timestamp,
                    h.height,
                    h.slot,
                    h.eligibilityCertificate.immutableBytes,
                    h.operationalCertificate.immutableBytes,
                    h.metadata.fold(Bytes.empty)(v => Bytes(v.data.bytes)),
                    h.address.immutableBytes
                  )
                )
              )
            )
        )
    }
  }

  implicit def byteVectorToByteString(byteVector: ByteVector): ByteString =
    ByteString.copyFrom(byteVector.toByteBuffer)

  implicit def byteStringToByteVector(byteString: ByteString): Bytes =
    ByteVector(byteString.asReadOnlyByteBuffer())
}
