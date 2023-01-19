package co.topl.catsakka

import cats.data.OptionT
import cats.effect.Async
import cats.effect.std.Queue
import cats.implicits._
import fs2._

object AbandonerPipe {

  /**
   * Constructs a Pipe which consumes the a stream and evaluates the function `f` on each element.  If the upstream
   * produces a new value before the function `f` completes, then the `f` is cancelled and a new attempt is made on the
   * new upstream value.
   * @param f a function to evaluate.  May be cancelled.
   * @tparam T input type
   * @tparam O output type
   */
  def apply[F[_]: Async, T, O](f: T => F[O]): Pipe[F, T, O] = {
    def nextOut(current: T, nextIn: => F[Option[T]]): F[Option[O]] =
      Async[F]
        .race(f(current), nextIn)
        .flatMap {
          case Left(o)           => o.some.pure[F]
          case Right(Some(next)) => nextOut(next, nextIn)
          case Right(None)       => none[O].pure[F]
        }

    in =>
      Stream
        .eval(Queue.circularBuffer[F, Option[T]](1))
        .flatMap { queue =>
          Stream
            .unfoldEval(queue)(queue =>
              OptionT(queue.take)
                .semiflatMap(nextOut(_, queue.take).tupleRight(queue))
                .value
            )
            .unNoneTerminate
            .concurrently(in.enqueueNoneTerminated(queue))
        }
  }
}
