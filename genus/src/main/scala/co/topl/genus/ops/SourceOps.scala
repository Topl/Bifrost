package co.topl.genus.ops

import akka.stream.Materializer
import akka.stream.scaladsl.{Sink, Source}
import cats.MonadThrow
import cats.effect.kernel.Async

import scala.concurrent.duration.FiniteDuration
import scala.language.implicitConversions

final class SourceOps[T, Mat](val source: Source[T, Mat]) {

  /**
   * Collects the given Source into a `Seq[T]` with a given allowed timeout.
   * @param timeout the amount of time to allow for the source to complete
   * @param materializer the stream's materializer
   * @tparam F an effect-ful type to wrap the resulting sequence with an instance of `Async` and `MonadThrow`
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

object SourceOps {

  trait ToSourceOps {
    implicit def fromSource[T, Mat](source: Source[T, Mat]): SourceOps[T, Mat] = new SourceOps(source)
  }
}
