package co.topl.catsutils

import cats.data.EitherT
import cats.effect.kernel.Async
import cats.effect.std.Queue
import cats.implicits._

/**
 * Represents the notion of an "iterative" process, or a process which can be continually invoked to improve upon a
 * value.
 */
trait Iterative[F[_], E] {

  /**
   * Make the given value better.  If a better value can't be produced, return `never`.  This doesn't need to be a
   * "drastic" improvement; any incremental improvement is valid (and generally preferred)
   * @param current the current/latest value to be improved
   * @return an improved value, if one can be made.  Otherwise, `never`
   */
  def improve(current: E): F[E]
}

object Iterative {

  /**
   * Continually invoke the given iterative on a "current state" until a signal is received to halt.  Upon receiving
   * the signal, the current best item is captured, the iterative process is cancelled, and the best item returned.
   */
  def run[F[_]: Async, E](init: F[E])(iterative: Iterative[F, E]): F[() => F[E]] =
    for {
      initialValue <- init
      // A queue which accepts a single signal from the outside indicating that the Iterative process should stop
      stopQueue <- Queue.bounded[F, Unit](1)
      // A queue which stores the "latest" result.  New results overwrite the previous.
      resultsQueue <- Queue.circularBuffer[F, E](1)
      // Store the provided initial value as the current best result.  If the consumer needs an immediate result,
      // the initial value is returned.
      _ <- resultsQueue.offer(initialValue)
      // Launch a fiber in the background which executes the Iterative process
      fiber <- Async[F].start {
        // Start from the initial value and recursively compute the next/improved value.  Once computed, the new
        // value is stored in the resultsQueue, and a check is performed to see if `stopQueue` instructs us to
        // exit the recursive loop
        initialValue.tailRecM(
          iterative
            .improve(_)
            .flatTap(resultsQueue.offer)
            .flatMap(r => EitherT.fromOptionF(stopQueue.tryTake, r).as(r).value)
        )
      }
    } yield {
      // Now return a function which sends a stop signal and retrieves the current/best result
      () => stopQueue.offer(()) >> resultsQueue.take.flatTap(_ => fiber.cancel)
    }

}
