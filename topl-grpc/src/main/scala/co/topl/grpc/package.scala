package co.topl

import cats.ApplicativeThrow
import cats.implicits._
import io.grpc.{Status, StatusException}

package object grpc {

  implicit class ThrowableAdapter(throwable: Throwable) {

    def asGrpcException: StatusException =
      throwable match {
        case e: StatusException => e
        case i: IllegalArgumentException =>
          Status.INVALID_ARGUMENT.withDescription(i.getMessage).asException()
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
