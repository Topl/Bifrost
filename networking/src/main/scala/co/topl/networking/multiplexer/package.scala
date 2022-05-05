package co.topl.networking

import akka.util.ByteString

import java.nio.ByteBuffer

package object multiplexer {

  def intToBytestring(value: Int): ByteString = ByteString(
    ByteBuffer.allocate(4).putInt(value).array()
  )

  def bytestringToInt(byteString: ByteString): Int =
    byteString.toByteBuffer.getInt
}
