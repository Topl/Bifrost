package co.topl.nodeCodecs.binary.legacy.network.message

import akka.util.ByteString
import co.topl.codecs.binary.legacy.{BifrostSerializer, Reader, Writer}
import co.topl.network.MaliciousBehaviorException
import co.topl.network.message.{Transmission, TransmissionContent, TransmissionHeader}

import java.nio.ByteOrder

/** Serializer for Message case class */
class TransmissionSerializer(magicBytes: Array[Byte]) extends BifrostSerializer[Transmission] {

  import scala.language.existentials

  implicit private val byteOrder: ByteOrder = ByteOrder.BIG_ENDIAN

  override def serialize(obj: Transmission, w: Writer): Unit = {
    w.putBytes(magicBytes)
    w.put(obj.header.code)

    // need to use Akka util Byte String putInt method for data length
    val bytes = ByteString.createBuilder.putInt(obj.header.dataLength).result().toArray
    w.putBytes(bytes)

    obj.content.foreach { content =>
      w.putBytes(content.checksum)
      w.putBytes(content.data)
    }
  }

  override def parse(r: Reader): Transmission = {

    val numBytes = r.remaining

    if (numBytes < Transmission.headerLength) {
      throw new Exception(
        s"Not enough bytes to parse transmission header: ${Transmission.headerLength} bytes required but $numBytes bytes given"
      )
    } else {
      val magic = r.getBytes(Transmission.magicLength)
      val msgCode = r.getByte()

      // need to use custom akka util ByteString getInt function
      val lengthBytes = r.getBytes(4)
      val length = ByteString(lengthBytes).iterator.getInt

      /** peer is from another network */
      if (!java.util.Arrays.equals(magic, magicBytes)) {
        throw MaliciousBehaviorException(s"Wrong magic bytes, expected ${magicBytes.mkString}, got ${magic.mkString}")
      }

      /** peer is trying to cause buffer overflow or breaking the parsing */
      if (length < 0) {
        throw MaliciousBehaviorException("Data length is negative!")
      }

      val content = if (length > 0) {
        val checksum = r.getBytes(Transmission.checksumLength)
        val data = r.getBytes(length)
        val digest = Transmission.checksum(data)

        /** peer reported incorrect checksum */
        if (!java.util.Arrays.equals(checksum, digest)) {
          throw MaliciousBehaviorException(
            s"Wrong checksum, expected ${digest.mkString}, got ${checksum.mkString}"
          )
        }
        Some(TransmissionContent(checksum, data))
      } else {
        None
      }

      Transmission(TransmissionHeader(msgCode, length), content)
    }
  }
}
