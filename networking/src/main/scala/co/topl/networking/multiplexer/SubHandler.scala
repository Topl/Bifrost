package co.topl.networking.multiplexer

import fs2._

/**
 * Consumes and produces messages for a particular session inside of a multiplexer
 */
case class SubHandler[F[_]](sessionId: Byte, subscriber: Pipe[F, Chunk[Byte], Unit], producer: Stream[F, Chunk[Byte]])
