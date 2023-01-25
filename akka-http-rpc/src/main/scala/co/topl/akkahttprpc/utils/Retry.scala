package co.topl.akkahttprpc.utils

import akka.actor.ActorSystem
import akka.stream.scaladsl.{Sink, Source}
import cats.data._

import scala.concurrent.Future
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.util.{Failure, Success}

object Retry {

  def apply[A, B](
    attempt:  () => EitherT[Future, A, B],
    interval: FiniteDuration,
    timeout:  FiniteDuration,
    attempts: Int
  )(implicit system: ActorSystem): EitherT[Future, A, B] = {
    import system.dispatcher
    EitherT(
      Source
        .tick(Duration.Zero, interval, {})
        .take(attempts)
        .takeWithin(timeout)
        .mapAsync(1)(_ => attempt().value.map(Success(_)).recover { case e => Failure(e) })
        .takeWhile(
          {
            case Success(Right(_)) => false
            case _                 => true
          },
          inclusive = true
        )
        .runWith(Sink.last)
        .flatMap(Future.fromTry)
    )
  }

}
