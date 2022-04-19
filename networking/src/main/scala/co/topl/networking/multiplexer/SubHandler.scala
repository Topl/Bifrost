package co.topl.networking.multiplexer

import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.util.ByteString

/**
 * Consumes and produces messages for a particular session inside of a multiplexer
 */
case class SubHandler(sessionId: Byte, subscriber: Sink[ByteString, _], producer: Source[ByteString, _])
