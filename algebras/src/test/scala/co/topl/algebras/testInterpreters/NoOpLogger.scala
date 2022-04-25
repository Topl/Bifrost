package co.topl.algebras.testInterpreters

import cats.Applicative
import org.typelevel.log4cats.Logger

class NoOpLogger[F[_]: Applicative] extends Logger[F] {
  def error(t: Throwable)(message: => String): F[Unit] = Applicative[F].unit

  def warn(t: Throwable)(message: => String): F[Unit] = Applicative[F].unit

  def info(t: Throwable)(message: => String): F[Unit] = Applicative[F].unit

  def debug(t: Throwable)(message: => String): F[Unit] = Applicative[F].unit

  def trace(t: Throwable)(message: => String): F[Unit] = Applicative[F].unit

  def error(message: => String): F[Unit] = Applicative[F].unit

  def warn(message: => String): F[Unit] = Applicative[F].unit

  def info(message: => String): F[Unit] = Applicative[F].unit

  def debug(message: => String): F[Unit] = Applicative[F].unit

  def trace(message: => String): F[Unit] = Applicative[F].unit
}
