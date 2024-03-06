package co.topl.catsutils

import cats.implicits._
import cats.effect._
import cats.effect.std.Queue
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.typelevel.log4cats.Logger

import scala.concurrent.duration._

class FOpsSpec extends CatsEffectSuite with ScalaCheckEffectSuite {

  type F[A] = IO[A]

  test("Should emit log messages if an operation is slow") {
    for {
      messages <- Queue.unbounded[F, String]
      implicit0(logger: Logger[F]) = new LoggerEnqueueWarnMessages(messages)
      _ <- Async[F].delayBy(().pure[F], 1.seconds).warnIfSlow("FOpsSpec", 100.milli, 100.milli)
      // Give an acceptable range since the clock/delay granularity may not be exact
      _ <- messages.size.map(size => size > 7 && size < 11).assert
    } yield ()
  }

  test("Should not emit log messages if an operation is fast enough") {
    for {
      messages <- Queue.unbounded[F, String]
      implicit0(logger: Logger[F]) = new LoggerEnqueueWarnMessages(messages)
      _ <- ().pure[F].warnIfSlow("FOpsSpec", 200.milli, 200.milli)
      _ <- messages.size.assertEquals(0)
    } yield ()
  }

  /**
   * A helper Logger due to issues with ScalaMock + call-by-name parameters
   */
  private class LoggerEnqueueWarnMessages(messages: Queue[F, String]) extends Logger[F] {
    def error(t: Throwable)(message: => String): F[Unit] = ???

    def warn(t: Throwable)(message: => String): F[Unit] = ???

    def info(t: Throwable)(message: => String): F[Unit] = ???

    def debug(t: Throwable)(message: => String): F[Unit] = ???

    def trace(t: Throwable)(message: => String): F[Unit] = ???

    def error(message: => String): F[Unit] = ???

    def warn(message: => String): F[Unit] = messages.offer(message)

    def info(message: => String): F[Unit] = ???

    def debug(message: => String): F[Unit] = ???

    def trace(message: => String): F[Unit] = ???
  }

}
