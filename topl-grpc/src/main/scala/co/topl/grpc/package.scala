package co.topl

import akka.grpc.GrpcServiceException
import cats.ApplicativeThrow
import cats.implicits._
import com.google.protobuf.ByteString
import io.grpc.Status
import org.typelevel.log4cats.Logger
import scodec.bits.ByteVector

import scala.language.implicitConversions

package object grpc extends Isomorphism.Ops with BifrostMorphismInstances {

  implicit def byteStringToByteVector(byteString: ByteString): ByteVector =
    ByteVector(byteString.asReadOnlyByteBuffer())

  implicit def byteVectorToByteString(byteVector: ByteVector): ByteString =
    ByteString.copyFrom(byteVector.toByteBuffer)

  implicit class ThrowableAdapter(throwable: Throwable) {

    def asGrpcException: GrpcServiceException =
      throwable match {
        case e: GrpcServiceException => e
        case i: IllegalArgumentException =>
          new GrpcServiceException(
            Status.INVALID_ARGUMENT.withDescription(i.getMessage)
          )
        case e: NotImplementedError =>
          new GrpcServiceException(
            Status.UNIMPLEMENTED.withDescription(e.getMessage)
          )
        case e =>
          new GrpcServiceException(Status.fromThrowable(e))
      }
  }

  implicit class FAdapter[F[_], A](fa: F[A]) {

    def adaptErrorsToGrpc(implicit applicativeThrowF: ApplicativeThrow[F]): F[A] =
      fa.adaptErr { case e =>
        e.asGrpcException
      }

    def logServerErrors(implicit loggerF: Logger[F], applicativeThrowF: ApplicativeThrow[F]): F[A] =
      fa.onError {
        case e => Logger[F].error(e)("gRPC Server Error")
      }
  }
}
