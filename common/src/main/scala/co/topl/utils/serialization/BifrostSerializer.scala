package co.topl.utils.serialization

import akka.util.ByteString

import java.nio.ByteBuffer
import scala.util.Try

trait BifrostSerializer[Content] extends Serializer[Content, Content, Reader, Writer] {

  def toByteString(obj: Content): ByteString = {
    val writer = new VLQByteStringWriter()
    serialize(obj, writer)
    writer.result()
  }

  def parseByteString(byteString: ByteString): Content = {
    val reader = new VLQByteStringReader(byteString)
    parse(reader)
  }

  def parseByteStringTry(byteString: ByteString): Try[Content] =
    Try(parseByteString(byteString))

  def toBytes(obj: Content): Array[Byte] = {
    val writer = new VLQByteBufferWriter(new ByteArrayBuilder())
    serialize(obj, writer)
    writer.result().toBytes
  }

  def parseBytes(bytes: Array[Byte]): Try[Content] = {
    val reader = new VLQByteBufferReader(ByteBuffer.wrap(bytes))
    Try(parse(reader))
  }
}
