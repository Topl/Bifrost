package co.topl.grpc

import akka.actor.ClassicActorSystemProvider
import akka.grpc.GrpcClientSettings
import akka.http.scaladsl.Http
import cats.MonadThrow
import cats.effect.kernel.{Async, Resource}
import cats.implicits._
import co.topl.algebras.ToplRpc
import co.topl.catsakka._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.grpc.services.{BroadcastTxReq, BroadcastTxRes, ToplGrpc}
import co.topl.models._
import com.google.protobuf.ByteString

import scala.concurrent.Future

object ToplGrpc {

  object Client {

    def make[F[_]: Async](host: String, port: Int)(implicit
      systemProvider:           ClassicActorSystemProvider
    ): F[ToplRpc[F]] =
      Async[F].delay {
        val client = services.ToplGrpcClient(GrpcClientSettings.connectToServiceAt(host, port).withTls(false))
        new ToplRpc[F] {
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
        }
      }
  }

  object Server {

    def serve[F[_]: Async: FToFuture](host: String, port: Int, interpreter: ToplRpc[F])(implicit
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

    private def grpcServerImpl[F[_]: MonadThrow: FToFuture](interpreter: ToplRpc[F]): ToplGrpc =
      new ToplGrpc {

        def broadcastTx(in: BroadcastTxReq): Future[BroadcastTxRes] =
          implicitly[FToFuture[F]].apply(
            Bytes(in.transmittableBytes.asReadOnlyByteBuffer())
              .decodeTransmitted[Transaction]
              .leftMap(err => new IllegalArgumentException(s"Invalid Transaction bytes. reason=$err"))
              .liftTo[F]
              .flatMap(interpreter.broadcastTx)
              .as(BroadcastTxRes())
          )
      }
  }
}
