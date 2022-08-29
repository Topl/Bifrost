package co.topl.catsakka

import cats.Monad
import cats.effect.Clock
import org.typelevel.log4cats.Logger

import scala.language.implicitConversions

trait FOps {

  implicit def faAsFAClockOps[F[_]: Monad: Clock: Logger, A](fa: F[A]): FAClockOps[F, A] =
    new FAClockOps(fa)
}

class FAClockOps[F[_], A](val fa: F[A]) extends AnyVal {
  import cats.implicits._

  def logDuration(operationName: String)(implicit fMonad: Monad[F], fClock: Clock[F], fLogger: Logger[F]): F[A] =
    Clock[F]
      .timed(fa)
      .flatMap { case (duration, result) =>
        Logger[F]
          .info(show"$operationName duration=$duration")
          .as(result)
      }
}
