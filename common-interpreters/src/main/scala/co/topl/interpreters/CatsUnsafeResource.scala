package co.topl.interpreters

import cats.data.OptionT
import cats.effect.Async
import cats.effect.std.Queue
import cats.implicits._
import co.topl.algebras.UnsafeResource

/**
 * An interpreter for the `UnsafeResource` algebra that is backed by a cats-effect queue.  Inactive resources
 * sit in a queue.  As requests arrive, a resource is dequeued, used, and then requeued.
 */
object CatsUnsafeResource {

  def make[F[_]: Async, T](init: => T, maxParallelism: Int): F[UnsafeResource[F, T]] =
    for {
      _     <- Async[F].raiseWhen(maxParallelism < 1)(new IllegalArgumentException("Invalid maxParallelism"))
      queue <- Queue.unbounded[F, Option[T]]
      // Launch with several uninitialized resources
      _ <- 0.iterateUntilM(i => queue.offer(None).as(i + 1))(_ >= maxParallelism)
      res = new UnsafeResource[F, T] {

        def use[Res](f: T => F[Res]): F[Res] =
          OptionT(queue.take)
            // If an uninitialized resource was pulled, initialize it
            .getOrElseF(Async[F].delay(init))
            // Now use the resource, and then re-queue it
            .flatMap(t => f(t).flatTap(_ => queue.offer(t.some)))
      }
    } yield res

}
