package co.topl.catsutils

import cats.data.EitherT
import cats.effect.implicits._
import cats.effect.kernel.Async
import cats.effect.{Deferred, Ref, Resource}
import cats.implicits._
import org.typelevel.log4cats.Logger

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
  def run[F[_]: Async: Logger, E](init: F[E])(iterative: Iterative[F, E]): Resource[F, F[E]] =
    for {
      initialValue <- init.toResource
      // A Deferred which accepts a single signal from the outside indicating that the Iterative process should stop
      stopDeferred <- Deferred[F, Either[Throwable, Unit]].toResource
      // A ref which stores the current best result
      result: Ref[F, E] <- Ref.of(initialValue).toResource
      _ <- fs2.Stream
        .repeatEval(
          result.get
            .flatTap(
              iterative
                .improve(_)
                .flatTap(result.set)
                .race(stopDeferred.get)
                .guarantee(Async[F].cede)
                .onError { case e => Logger[F].error(e)("Iterative process failed") }
            )
        )
        .interruptWhen(stopDeferred)
        .compile
        .drain
        .background
    } yield
    // Now return a function which sends a stop signal and retrieves the current/best result
    Async[F].defer(stopDeferred.complete(().asRight) >> result.get)

}
