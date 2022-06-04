package co.topl.grpc

import akka.NotUsed
import akka.actor.ClassicActorSystemProvider
import akka.grpc.GrpcClientSettings
import akka.http.scaladsl.Http
import akka.stream.scaladsl.Source
import cats.MonadThrow
import cats.effect.kernel.{Async, Resource}
import cats.implicits._
import co.topl.algebras.ToplRpc
import co.topl.catsakka._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.grpc.services.{BlockAdoptionsReq, BlockAdoptionsRes, FetchBlockHeaderReq, FetchBlockHeaderRes}
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

    def make[F[_]: Async](host: String, port: Int)(implicit
      systemProvider:           ClassicActorSystemProvider
    ): F[ToplRpc[F, Source[*, NotUsed]]] =
      Async[F].delay {
        val client = services.ToplGrpcClient(GrpcClientSettings.connectToServiceAt(host, port).withTls(false))
        new ToplRpc[F, Source[*, NotUsed]] {
          def broadcastTx(transaction: Transaction): F[Unit] =
            Async[F]
              .fromFuture(
                Async[F].delay(
                  client.broadcastTx(
                    services.BroadcastTxReq(
                      ByteString.copyFrom(
                        transaction.immutableBytes.toByteBuffer
                      )
                    )
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
        }
      }
  }

  object Server {

    def serve[F[_]: Async: FToFuture](host: String, port: Int, interpreter: ToplRpc[F, Source[*, NotUsed]])(implicit
      systemProvider:                       ClassicActorSystemProvider
    ): Resource[F, Http.ServerBinding] =
      Resource.make(
        Async[F].fromFuture(
          Async[F].delay(
            Http()
              .newServerAt(host, port)
              .bind(services.ToplGrpcHandler(grpcServerImpl[F](interpreter)))
          )
        )
      )(binding => Async[F].fromFuture(Async[F].delay(binding.unbind())).void)

    private def grpcServerImpl[F[_]: MonadThrow: FToFuture](
      interpreter: ToplRpc[F, Source[*, NotUsed]]
    ): services.ToplGrpc =
      new services.ToplGrpc {

        def broadcastTx(in: services.BroadcastTxReq): Future[services.BroadcastTxRes] =
          implicitly[FToFuture[F]].apply(
            Bytes(in.transmittableBytes.asReadOnlyByteBuffer())
              .decodeTransmitted[Transaction]
              .leftMap(err => new IllegalArgumentException(s"Invalid Transaction bytes. reason=$err"))
              .liftTo[F]
              .flatMap(interpreter.broadcastTx)
              .as(services.BroadcastTxRes())
          )

        def blockAdoptions(in: BlockAdoptionsReq): Source[BlockAdoptionsRes, NotUsed] =
          Source
            .futureSource(implicitly[FToFuture[F]].apply(interpreter.blockAdoptions()))
            .map(_.allBytes.toByteBuffer)
            .map(ByteString.copyFrom)
            .map(BlockAdoptionsRes(_))
            .mapMaterializedValue(_ => NotUsed)

        def fetchBlockHeader(in: FetchBlockHeaderReq): Future[FetchBlockHeaderRes] =
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
