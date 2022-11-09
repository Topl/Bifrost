package co.topl.catsakka

import akka.NotUsed
import akka.stream.scaladsl.{Keep, Sink, Source}
import akka.stream.{BoundedSourceQueue, KillSwitches, Materializer, OverflowStrategy, QueueOfferResult}
import cats.effect.Async
import cats.effect.kernel.Sync
import cats.implicits._
import cats.kernel.Monoid
import cats.{Applicative, Functor, MonadThrow}

import fs2.interop.reactivestreams._
import fs2._

import scala.language.implicitConversions

trait SourceOps {

  type SourceMatNotUsed[T] = Source[T, NotUsed]

  implicit def sourceAsSourceCatsOps[T, Mat](source: Source[T, Mat]): SourceCatsOps[T, Mat] =
    new SourceCatsOps(source)

  implicit def boundedSourceQueueAsBoundedSourceQueueOps[T](queue: BoundedSourceQueue[T]): BoundedSourceQueueOps[T] =
    new BoundedSourceQueueOps[T](queue)

  implicit def sourceCompanionAsSourceCompanionOps(companion: Source.type): SourceCompanionCatsOps =
    new SourceCompanionCatsOps(companion)

  def handleQueueOfferResult[F[_]: MonadThrow](queueOfferResult: QueueOfferResult): F[Unit] =
    queueOfferResult match {
      case QueueOfferResult.Enqueued =>
        Applicative[F].unit
      case QueueOfferResult.Dropped =>
        MonadThrow[F].raiseError(new IllegalStateException("Downstream too slow")).void
      case QueueOfferResult.QueueClosed =>
        MonadThrow[F].raiseError(new IllegalStateException("Queue closed")).void
      case QueueOfferResult.Failure(e) =>
        MonadThrow[F].raiseError(e).void
    }

  implicit def sourceMonoid[T]: Monoid[Source[T, NotUsed]] =
    Monoid.instance[Source[T, NotUsed]](Source.empty, (s1, s2) => s1.merge(s2))

  implicit val sourceFunctor: Functor[SourceMatNotUsed] =
    new Functor[SourceMatNotUsed] {
      def map[A, B](fa: SourceMatNotUsed[A])(f: A => B): SourceMatNotUsed[B] = fa.map(f)
    }

  implicit val sourceApplicative: Applicative[SourceMatNotUsed] =
    new Applicative[SourceMatNotUsed] {
      def pure[A](x: A): SourceMatNotUsed[A] = Source.single(x)

      def ap[A, B](ff: SourceMatNotUsed[A => B])(fa: SourceMatNotUsed[A]): SourceMatNotUsed[B] =
        fa.flatMapConcat(a => ff.map(_.apply(a)))
    }

}

class SourceCatsOps[T, Mat](val source: Source[T, Mat]) extends AnyVal {

  /**
   * Evaluates the given `f` function on each element of the stream
   */
  def mapAsyncF[F[_]: FToFuture, U](parallelism: Int)(f: T => F[U]): Source[U, Mat] =
    source.map(f).mapAsync(parallelism)(implicitly[FToFuture[F]].apply)

  /**
   * Perform a side-effecting operation on the given values in the F context
   */
  def tapAsyncF[F[_]: Functor: FToFuture](parallelism: Int)(f: T => F[Unit]): Source[T, Mat] =
    source.map(a => f(a).as(a)).mapAsync(parallelism)(implicitly[FToFuture[F]].apply)

  /**
   * Inserts MonadCancel#onCancel support into this Source
   */
  def withCancel[F[_]: Async](implicit materializer: Materializer): F[Source[T, Mat]] =
    Sync[F]
      .delay(source.viaMat(KillSwitches.single)(Keep.both).preMaterialize())
      .flatMap { case ((mat, killSwitch), source) =>
        Async[F].onCancel(source.mapMaterializedValue(_ => mat).pure[F], Sync[F].delay(killSwitch.shutdown()))
      }

  /**
   *  Akka streams to FS2 Stream
   *  @see [[fs2.interop.reactivestreams.fromPublisher]]
   */
  def asFS2Stream[F[_]: Async](implicit materializer: Materializer): Stream[F, T] =
    Stream
      .eval(
        Async[F].delay(
          source
            .runWith(Sink.asPublisher[T](fanout = false))
            .toStreamBuffered[F](bufferSize = 1)
        )
      )
      .flatten

}

class BoundedSourceQueueOps[T](val queue: BoundedSourceQueue[T]) extends AnyVal {
  def offerF[F[_]: MonadThrow](t: T): F[Unit] = handleQueueOfferResult[F](queue.offer(t))
}

class SourceCompanionCatsOps(val companion: Source.type) extends AnyVal {

  /**
   * Constructs a backpressured Akka Stream Source that can be interacted with using the materialized functions
   * @param size The buffer size of the queue
   * @tparam F F-context
   * @tparam T Value Type
   * @return a Source[T] where the materialized value is a tuple (offer to queue function, complete queue function)
   */
  def backpressuredQueue[F[_]: Async, T](size: Int = 16): Source[T, (T => F[Unit], Option[Throwable] => F[Unit])] =
    companion
      .queue[T](size, OverflowStrategy.backpressure, maxConcurrentOffers = size)
      .mapMaterializedValue(queue =>
        (
          (t: T) => Async[F].fromFuture(Async[F].delay(queue.offer(t))).flatMap(handleQueueOfferResult[F]),
          (reason: Option[Throwable]) => Async[F].delay(reason.fold(queue.complete())(queue.fail))
        )
      )

  /**
   * Constructs a Akka Stream Source that discards old elements to keep up with demand.
   * It can be interacted with using the materialized functions
   * @param size The buffer size of the queue
   * @tparam F F-context
   * @tparam T Value Type
   * @return a Source[T] where the materialized value is a tuple (offer to queue function, complete queue function)
   */
  def dropHeadQueue[F[_]: Async, T](size: Int = 16): Source[T, (T => F[Unit], Option[Throwable] => F[Unit])] =
    companion
      .queue[T](size, OverflowStrategy.dropHead)
      .mapMaterializedValue(queue =>
        (
          (t: T) => Async[F].fromFuture(Async[F].delay(queue.offer(t))).flatMap(handleQueueOfferResult[F]),
          (reason: Option[Throwable]) => Async[F].delay(reason.fold(queue.complete())(queue.fail))
        )
      )

  /**
   * Constructs a Akka Stream Source that discards old elements to keep up with demand.
   * It can be interacted with using the materialized functions
   * @param size The buffer size of the queue
   * @tparam F F-context
   * @tparam T Value Type
   * @return a Source[T] where the materialized value is a tuple (offer to queue function, complete queue function)
   */
  def failQueue[F[_]: Async, T](size: Int = 16): Source[T, (T => F[Unit], Option[Throwable] => F[Unit])] =
    companion
      .queue[T](size, OverflowStrategy.fail)
      .mapMaterializedValue(queue =>
        (
          (t: T) => Async[F].fromFuture(Async[F].delay(queue.offer(t))).flatMap(handleQueueOfferResult[F]),
          (reason: Option[Throwable]) => Async[F].delay(reason.fold(queue.complete())(queue.fail))
        )
      )
}
