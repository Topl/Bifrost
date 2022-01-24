package co.topl.genus.extensions

import akka.NotUsed
import akka.stream.Materializer
import akka.stream.scaladsl.{Sink, Source}
import cats.MonadThrow
import cats.effect.kernel.Async

import scala.concurrent.duration.FiniteDuration

trait Extensions {

  implicit final class SourceExtensions[T](val source: Source[T, NotUsed]) {

    /**
     * Collects the given Source into a `Seq[T]` with a given allowed timeout.
     * @param timeout the amount of time to allow for the source to complete
     * @param materializer the stream's materializer
     * @tparam F a functor to wrap the result sequence with an instance of `Async`
     * @return a result of `F[Seq[T]]` with the possibility of a `TimeoutException` if the source
     *         does not complete before the given timeout
     */
    def collectWithTimeout[F[_]: Async: MonadThrow](timeout: FiniteDuration)(implicit
      materializer:                                          Materializer
    ): F[Seq[T]] =
      Async[F].fromFuture(
        MonadThrow[F]
          .catchNonFatal(source.completionTimeout(timeout).runWith(Sink.seq))
      )
  }
}
