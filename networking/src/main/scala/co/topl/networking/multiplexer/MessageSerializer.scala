package co.topl.networking.multiplexer

import akka.NotUsed
import akka.stream.scaladsl.Flow
import akka.util.ByteString

trait MessageSerializer[T] extends (T => (Byte, ByteString))

object MessageSerializerFramer {

  def apply(): Flow[(Byte, ByteString), ByteString, NotUsed] =
    Flow[(Byte, ByteString)]
      .map { case (typeByte, data) =>
        ByteString(typeByte) ++ intToBytestring(data.length) ++ data
      }
}
