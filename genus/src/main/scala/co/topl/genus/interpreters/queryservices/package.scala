package co.topl.genus.interpreters

import akka.NotUsed
import akka.stream.Materializer
import akka.stream.scaladsl.{Sink, Source}
import cats.effect.kernel.Async
import cats.{~>, MonadThrow}

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

package object queryservices {

  type ToFuture[F[_]] = F ~> Future

  trait Implicits {

    implicit final class SourceExtensions[T](val source: Source[T, NotUsed]) {

      /**
       * Collects the given Source into a `Seq[T]` with a given allowed timeout.
       * @param timeout the amount of time to allow for the source to complete
       * @param materializer the stream's materializer
       * @param monadError an instance of `MonadError[*, Throwable]`
       * @tparam F a functor to wrap the result sequence with an instance of `Async`
       * @return a result of `F[Seq[T]]` with the possibility of a `TimeoutException` if the source
       *         does not complete before the given timeout
       */
      def collectWithTimeout[F[_]: Async: MonadThrow](timeout: FiniteDuration)(implicit
        materializer:                                          Materializer
      ): F[Seq[T]] =
        Async[F].fromFuture(
          MonadThrow[F].catchNonFatal(
            source.completionTimeout(timeout).runWith(Sink.seq)
          )
        )
    }
  }

  object implicits extends Implicits
}
