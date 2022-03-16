package co.topl.networking.multiplexer

import akka.stream.scaladsl.Flow
import akka.util.ByteString

case class SubHandler(sessionId: Byte, handler: Flow[ByteString, ByteString, _])
