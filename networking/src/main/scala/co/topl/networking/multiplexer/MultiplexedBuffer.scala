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
   * Handle a response from the peer.
   * @return true if the response was expected, false if the response was unexpected (i.e. response without a request)
   */
  def processResponse(response: Response)(implicit monadThrow: MonadThrow[F]): F[Boolean] =
    OptionT(responses.tryTake)
      .semiflatMap(_.complete(response))
      .isDefined

  /**
   * A background-handler for incoming requests from the peer
   * @param subProcessor a function which fulfills the peer's request
   */
  def backgroundRequestProcessor(subProcessor: Request => F[Unit])(implicit async: Async[F]): Stream[F, Unit] =
    Stream
      .fromQueueUnterminated(requests)
      .evalMap(subProcessor)

  /**
   * Create and enqueue a placeholder Deferred, which is completed when the remote peer fulfills the request.
   *
   * The returned outer F[_] is the effect of enqueing a response.
   * The nested inner F[_] is the effect of awaiting the response.
   *
   * @return a new effect that can be run to await the actual response
   */
  def expectResponse(implicit async: Async[F]): F[F[Response]] =
    Deferred[F, Response].flatTap(responses.offer).map(_.get)
}

object MultiplexedBuffer {

  def make[F[_]: Async, Request, Response]: Resource[F, MultiplexedBuffer[F, Request, Response]] =
    (
      Queue.unbounded[F, Request].toResource,
      Queue.unbounded[F, Deferred[F, Response]].toResource
    )
      .mapN(MultiplexedBuffer.apply)
}
