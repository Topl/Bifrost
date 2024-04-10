package co.topl.networking.multiplexer

import cats.MonadThrow
import cats.data.OptionT
import cats.effect.implicits._
import cats.effect.std.Queue
import cats.effect.{Async, Deferred, Resource}
import cats.implicits._
import fs2.Stream

/**
 * Serves as a buffer for both inbound and outbound messages over a specific multiplexer port
 * @param requests a queue of outbound messages to be sent to the peer
 * @param responses a queue of callbacks to be invoked upon receiving inbound messages from the peer
 */
case class MultiplexedBuffer[F[_], Request, Response](
  requests:  Queue[F, Request],
  responses: Queue[F, Deferred[F, Response]]
) {

  /**
   * Send a new request to the peer
   */
  def processRequest(request: Request): F[Unit] =
    requests.offer(request)

  /**
   * Handle a response from the peer
   */
  def processResponse(response: Response)(implicit monadThrow: MonadThrow[F]): F[Unit] =
    OptionT(responses.tryTake)
      .getOrRaise(new IllegalStateException("Unexpected Response"))
      .flatMap(_.complete(response))
      .void

  /**
   * A background-handler for incoming requests from the peer
   * @param subProcessor a function which fulfills the peer's request
   */
  def backgroundRequestProcessor(
    subProcessor: Request => F[Unit]
  )(implicit async: Async[F]): Stream[F, Unit] =
    Stream
      .fromQueueUnterminated(requests)
      .evalMap(subProcessor)

  /**
   * Create and enqueue a placeholder Deferred, which is completed when the remote peer fulfills the request
   * @return the resulting response, once fulfilled by the remote peer
   */
  def createResponse(implicit async: Async[F]): F[Response] =
    Deferred[F, Response].flatTap(responses.offer).flatMap(_.get)
}

object MultiplexedBuffer {

  def make[F[_]: Async, Request, Response]: Resource[F, MultiplexedBuffer[F, Request, Response]] =
    (
      Queue.unbounded[F, Request].toResource,
      Queue
        .unbounded[F, Deferred[F, Response]]
        .toResource
    )
      .mapN(MultiplexedBuffer.apply)
}
