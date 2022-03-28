package co.topl.catsakka

import akka.NotUsed
import akka.stream.scaladsl.Source
import akka.stream.{BoundedSourceQueue, OverflowStrategy, QueueOfferResult}
import cats.effect.Async
import cats.implicits._
import cats.kernel.Monoid
import cats.{Applicative, Functor, MonadThrow}

trait SourceOps {

  type SourceMatNotUsed[T] = Source[T, NotUsed]

  implicit class SourceCatsOps[T, Mat](source: Source[T, Mat]) {

    def mapAsyncF[F[_]: FToFuture, U](parallelism: Int)(f: T => F[U]): Source[U, Mat] =
      source.map(f).mapAsync(parallelism)(implicitly[FToFuture[F]].apply)

    def tapAsyncF[F[_]: Functor: FToFuture](parallelism: Int)(f: T => F[Unit]): Source[T, Mat] =
      source.map(a => f(a).as(a)).mapAsync(parallelism)(implicitly[FToFuture[F]].apply)
  }

  implicit class BoundedSourceQueueOps[T](queue: BoundedSourceQueue[T]) {
    def offerF[F[_]: MonadThrow](t: T): F[Unit] = handleQueueOfferResult[F](queue.offer(t))
  }

  private def handleQueueOfferResult[F[_]: MonadThrow](queueOfferResult: QueueOfferResult) =
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

  implicit class SourceCompanionCatsOps(companion: Source.type) {

    /**
     * Constructs a backpressured Akka Stream Source that can be interacted with using the materialized functions
     * @param size The buffer size of the queue
     * @tparam F F-context
     * @tparam T Value Type
     * @return a Source[T] where the materialized value is a tuple (offer to queue function, complete queue function)
     */
    def backpressuredQueue[F[_]: Async, T](size: Int = 16): Source[T, (T => F[Unit], Option[Throwable] => F[Unit])] =
      companion
        .queue[T](16, OverflowStrategy.backpressure)
        .mapMaterializedValue(queue =>
          (
            (t: T) => Async[F].fromFuture(Async[F].delay(queue.offer(t))).flatMap(handleQueueOfferResult[F]),
            (reason: Option[Throwable]) => Async[F].delay(reason.fold(queue.complete())(queue.fail))
          )
        )
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
