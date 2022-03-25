package co.topl.catsakka

import akka.NotUsed
import akka.stream.{BoundedSourceQueue, QueueOfferResult}
import akka.stream.scaladsl.Source
import cats.kernel.Monoid
import cats.{~>, Applicative, Functor, MonadThrow}
import cats.implicits._

import scala.concurrent.Future

trait SourceOps {

  implicit def akkaSourceMonoid[T]: Monoid[Source[T, NotUsed]] =
    Monoid.instance[Source[T, NotUsed]](Source.empty, (s1, s2) => s1.merge(s2))

  implicit class SourceCatsOps[T, Mat](source: Source[T, Mat]) {

    def mapAsyncF[F[_]: *[_] ~> Future, U](parallelism: Int)(f: T => F[U]): Source[U, Mat] =
      source.map(f).mapAsync(parallelism)(implicitly[F ~> Future].apply)

    def tapAsyncF[F[_]: Functor: *[_] ~> Future](parallelism: Int)(f: T => F[Unit]): Source[T, Mat] =
      source.map(a => f(a).as(a)).mapAsync(parallelism)(implicitly[F ~> Future].apply)
  }

  implicit class BoundedSourceQueueOps[T](queue: BoundedSourceQueue[T]) {

    def offerF[F[_]: MonadThrow](t: T): F[Unit] = queue.offer(t) match {
      case QueueOfferResult.Enqueued =>
        Applicative[F].unit
      case QueueOfferResult.Dropped =>
        MonadThrow[F].raiseError(new IllegalStateException("Downstream too slow")).void
      case QueueOfferResult.QueueClosed =>
        MonadThrow[F].raiseError(new IllegalStateException("Queue closed")).void
      case QueueOfferResult.Failure(e) =>
        MonadThrow[F].raiseError(e).void
    }
  }

}
