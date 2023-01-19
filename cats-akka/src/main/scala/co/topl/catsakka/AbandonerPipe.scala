package co.topl.catsakka

import cats.data.EitherT
import cats.effect.Async
import cats.effect.kernel.Sync
import cats.effect.std.Queue
import cats.implicits._
import fs2._

object AbandonerPipe {

  /**
   * Constructs a Pipe which consumes the a stream and evaluates the function `f` on each element.  If the upstream
   * produces a new value before the function `f` completes, then the `f` is cancelled and a new attempt is made on the
   * new upstream value.
   * @param f a function to evaluate.  May be cancelled.
   * @tparam I input type
   * @tparam O output type
   */
  def apply[F[_]: Async, I, O](f: I => F[O]): Pipe[F, I, O] = {

    /**
     * Starts a race between:
     *   - Dequeining next element from upstream
     *   - Processing current element against `f`
     * If upstream produces a new value first, then the current processing is cancelled.
     * If the `f` function completes first, it returns its value
     * @param current The current (most recent) value from upstream
     * @param nextIn an effect which reads the next element from upstream, possibly waiting indefinitely
     */
    def nextOut(current: I, nextIn: => F[I]): F[O] =
      EitherT(Async[F].race(nextIn, f(current)))
        .valueOrF(nextOut(_, nextIn))

    in =>
      Stream
        .eval(Queue.unbounded[F, I])
        .flatMap(queue =>
          Stream
            .constant[F, Unit](())
            .evalMap(_ => queue.take.flatMap(nextOut(_, Sync[F].defer(queue.take))))
            .mergeHaltR(in.enqueueUnterminated(queue))
        )
  }
}
