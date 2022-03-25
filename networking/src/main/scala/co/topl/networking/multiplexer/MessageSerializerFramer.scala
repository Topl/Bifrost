package co.topl.networking.multiplexer

import akka.NotUsed
import akka.stream.scaladsl.Flow
import akka.util.ByteString

/**
 * An Akka Flow which serializes "typed data" (meaning, data bytes which with a byte prefix indicating the data's type).
 *
 * The data is formatted as: prefix + data length + data
 */
object MessageSerializerFramer {

  def apply(): Flow[(Byte, ByteString), ByteString, NotUsed] =
    Flow[(Byte, ByteString)]
      .map { case (typeByte, data) =>
        ByteString(typeByte) ++ intToBytestring(data.length) ++ data
      }
}
