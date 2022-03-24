package co.topl.utils

import cats.data.Validated
import scodec.Attempt

import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

private[topl] object IdiomaticScalaTransition {

  trait ValidatedOps[L, R] {
    def instance: Validated[L, R]

    def getOrThrow(orThrow: L => Throwable = l => new Exception(l.toString)): R =
      instance match {
        case Validated.Valid(a)   => a
        case Validated.Invalid(e) => throw orThrow(e)
      }
  }

  trait ToValidatedOps {

    implicit def toValidatedOps[L, R](v: Validated[L, R]): ValidatedOps[L, R] = new ValidatedOps[L, R] {
      def instance: Validated[L, R] = v
    }
  }

  trait EitherOps[L, R] {
    def instance: Either[L, R]

    def getOrThrow(orThrow: L => Throwable = l => new Exception(l.toString)): R =
      instance match {
        case Right(a) => a
        case Left(e)  => throw orThrow(e)
      }
  }

  trait ToEitherOps {

    implicit def toEitherOps[L, R](v: Either[L, R]): EitherOps[L, R] = new EitherOps[L, R] {
      def instance: Either[L, R] = v
    }
  }

  trait TryOps[R] {
    def instance: Try[R]

    def getOrThrow(): R =
      instance match {
        case Success(a) => a
        case Failure(e) => throw e
      }
  }

  trait ToTryOps {

    implicit def toTryOps[R](v: Try[R]): TryOps[R] = new TryOps[R] {
      def instance: Try[R] = v
    }
  }

  trait AttemptOps[R] {
    def instance: Attempt[R]

    def getOrThrow(): R =
      instance match {
        case Attempt.Successful(a) => a
        case Attempt.Failure(e)    => throw new Exception(e.messageWithContext)
      }
  }

  trait ToAttemptOps {

    implicit def toAttemptOps[R](v: Attempt[R]): AttemptOps[R] = new AttemptOps[R] {
      def instance: Attempt[R] = v
    }
  }

  object implicits extends ToValidatedOps with ToEitherOps with ToTryOps with ToAttemptOps
}
