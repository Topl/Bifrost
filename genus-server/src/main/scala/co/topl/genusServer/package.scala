package co.topl

import cats.ApplicativeThrow
import cats.implicits._
import co.topl.genusLibrary.model.GREs
import io.grpc.{Status, StatusException}

/**
 * This package contains logic to run a Genus server. The Genus logic that the server runs comes from the genus-library
 * module.
 */
package object genusServer {

  implicit class ThrowableAdapter(throwable: Throwable) {

    def asGrpcException: StatusException =
      throwable match {
        case e: GREs.NotFound =>
          Status.NOT_FOUND.augmentDescription(s"${e.getMessage}").asException()
        case e: GREs.UnImplemented.type =>
          Status.UNIMPLEMENTED.augmentDescription(s"${e.getMessage}").asException()
        case e: GREs.Internal =>
          Status.INTERNAL.augmentDescription(s"${e.getMessage}").asException()
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
