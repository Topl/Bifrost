package co.topl.networking.multiplexer

import cats.MonadThrow
import cats.data.OptionT
import cats.effect.implicits._
import cats.effect.std.Queue
import cats.effect.{Async, Deferred, Resource}
import cats.implicits._
import fs2.Stream

case class PortQueues[F[_], Request, Response](
  requests:  Queue[F, Request],
  responses: Queue[F, Deferred[F, Response]]
) {

  def processRequest(request: Request): F[Unit] =
    requests.offer(request)

  def processResponse(response: Response)(implicit monadThrow: MonadThrow[F]): F[Unit] =
    OptionT(responses.tryTake)
      .getOrRaise(new IllegalStateException("Unexpected Response"))
      .flatMap(_.complete(response))
      .void

  def backgroundRequestProcessor(
    subProcessor: Request => F[Unit]
  )(implicit async: Async[F]): Stream[F, Unit] =
    Stream
      .fromQueueUnterminated(requests)
      .evalMap(subProcessor)

  def createResponse(implicit async: Async[F]): F[Response] =
    Deferred[F, Response].flatTap(responses.offer).flatMap(_.get)
}

object PortQueues {

  def make[F[_]: Async, Request, Response]: Resource[F, PortQueues[F, Request, Response]] =
    (
      Queue.unbounded[F, Request].toResource,
      Queue
        .unbounded[F, Deferred[F, Response]]
        .toResource
    )
      .mapN(PortQueues.apply)
}
