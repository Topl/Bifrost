package co.topl

import cats.ApplicativeThrow
import cats.effect.{Resource, Sync}
import cats.implicits._
import io.grpc.netty.shaded.io.grpc.netty.NettyChannelBuilder
import io.grpc.{ManagedChannel, Status, StatusException}
import scalapb.validate.FieldValidationException
import fs2.grpc.syntax.all._

package object grpc {

  def makeChannel[F[_]: Sync](host: String, port: Int, tls: Boolean): Resource[F, ManagedChannel] = {
    val base = NettyChannelBuilder.forAddress(host, port)

    val withTls =
      if (tls) base.useTransportSecurity() else base.usePlaintext()

    withTls.resource[F]
  }

  implicit class ThrowableAdapter(throwable: Throwable) {

    def asGrpcException: StatusException =
      throwable match {
        case e: StatusException => e
        case i: IllegalArgumentException =>
          Status.INVALID_ARGUMENT.withDescription(i.getMessage).asException()
        case f: FieldValidationException =>
          Status.INVALID_ARGUMENT.withDescription(f.getMessage).asException()
        case e: NotImplementedError =>
          Status.UNIMPLEMENTED.withDescription(e.getMessage).asException()
        case e =>
          Status.fromThrowable(e).asException()
      }
  }

  implicit class FApplicativeErrorAdapter[F[_]: ApplicativeThrow, A](fa: F[A]) {

    def adaptErrorsToGrpc: F[A] =
      fa.adaptErr { case e =>
        e.asGrpcException
      }
  }
}
