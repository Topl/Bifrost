package co.topl.catsakka

import akka.actor.typed.ActorSystem
import cats.data.EitherT
import cats.effect.kernel.Async
import cats.effect.std.Queue
import cats.implicits._

/**
 * Represents the notion of an "iterative" process, or a process which can be continually invoked to improve upon a
 * value.
 */
trait Iterative[F[_], E] {
  def improve(current: E): F[E]
}

object Iterative {

  /**
   * Continually invoke the given iterative on a "current state" until a signal is received to halt.  Upon receiving
   * the signal, the current best item is captured, the iterative process is cancelled, and the best item returned.
   */
  def run[F[_]: Async: FToFuture, E](init: F[E])(iterative: Iterative[F, E])(implicit
    system:                                ActorSystem[_]
  ): F[() => F[E]] =
    for {
      initialValue <- init
      stopQueue    <- Queue.bounded[F, Unit](1)
      resultsQueue <- Queue.dropping[F, E](1)
      _            <- resultsQueue.offer(initialValue)
      fiber <- Async[F].start {
        initialValue.tailRecM(
          iterative
            .improve(_)
            .flatTap(resultsQueue.offer)
            .flatMap(r => EitherT.fromOptionF(stopQueue.tryTake, r).as(r).value)
        )
      }
    } yield () => stopQueue.offer(()) >> resultsQueue.take.flatTap(_ => fiber.cancel)

}
