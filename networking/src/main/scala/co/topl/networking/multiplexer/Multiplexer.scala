package co.topl.networking.multiplexer

import cats.data.NonEmptyChain
import cats.effect.Async
import cats.effect.Resource
import cats.effect.implicits._
import cats.effect.std.Queue
import cats.implicits._
import fs2._
import fs2.io.net.Socket

/**
 * Multiplexes outbound sub-protocol "packets" into a single stream.  Demultiplexes inbound "packets" into multiple
 * sub-protocols.
 *
 * In this case, a "packet" is similar to the TCP notion of a packet, but is meant to be a layer up from
 * the low-level TCP packets.
 *
 * Each inbound "packet" is expected to be in the form of (byte prefix, int length, data).  The packet is read in full
 * before being forwarded onto the sub-protocol matching the packet's byte prefix.
 *
 * Each outbound "packet" is placed into the form of (byte prefix, int length, data).  When a sub-protocol produces data,
 * the multiplexer prepends the sub-protocol's byte prefix and the length of the data.
 */
object Multiplexer {

  def apply[F[_]: Async](subProtocols: NonEmptyChain[SubHandler[F]])(socket: Socket[F]): Resource[F, Unit] =
    for {
      queues <- subProtocols
        .traverse(p =>
          Queue
            .unbounded[F, Chunk[Byte]]
            .toResource
            .tupleLeft(p.sessionId)
        )
        .map(_.toIterable.toMap)
      queueProcessors = Stream
        .foldable(subProtocols)
        .map(p =>
          Stream
            .fromQueueUnterminated(queues(p.sessionId))
            .through(p.subscriber)
        )
        .parJoinUnbounded
        .compile
        .drain
        .toResource
      inputProcessor = socket.reads
        .through(MessageParserFramer())
        .evalTap { case (session, bytes) =>
          queues(session).offer(bytes)
        }
        .compile
        .drain
        .toResource
      outputProcessor = Stream
        .foldable(subProtocols)
        .map(h => h.producer.tupleLeft(h.sessionId))
        .parJoinUnbounded
        .through(MessageSerializerFramer())
        .unchunks
        .through(socket.writes)
        .compile
        .drain
        .toResource
      _ <- (queueProcessors, inputProcessor, outputProcessor).parTupled
    } yield ()

}
