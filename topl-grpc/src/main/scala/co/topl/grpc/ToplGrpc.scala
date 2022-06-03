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
import co.topl.grpc.services.{BlockAdoptionsReq, BlockAdoptionsRes}
import co.topl.models._
import com.google.protobuf.ByteString
import scodec.bits.ByteVector

import scala.concurrent.Future

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
      }
  }
}
