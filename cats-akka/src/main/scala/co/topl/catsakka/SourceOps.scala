package co.topl.catsakka

import akka.NotUsed
import akka.stream.{BoundedSourceQueue, QueueOfferResult}
import akka.stream.scaladsl.Source
import cats.kernel.Monoid
import cats.{~>, Applicative, Eval, Foldable, Functor, MonadThrow, Semigroup}
import cats.implicits._

import scala.concurrent.Future

trait SourceOps {

  type SourceMatNotUsed[T] = Source[T, NotUsed]

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
