package co.topl.networking.multiplexer

import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.util.ByteString

case class SubHandler(sessionId: Byte, subscriber: Sink[ByteString, _], producer: Source[ByteString, _])
