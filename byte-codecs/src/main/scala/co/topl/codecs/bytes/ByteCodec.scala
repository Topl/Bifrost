package co.topl.codecs.bytes

import co.topl.models.Bytes
import simulacrum.{op, typeclass}

import java.nio.ByteBuffer

@typeclass trait ByteCodec[T] {
  @op("writeBytesTo") def encode(t: T, writer: Writer): Unit
  def decode(reader:                Reader): T

  @op("bytes") def bytesOf(t: T): Bytes = {
    val writer = new VLQByteBufferWriter(new ByteArrayBuilder())
    encode(t, writer)
    Bytes(writer.toBytes)
  }

}

object ByteCodec {

  trait Instances {

    implicit class BytesOps(bytes: Bytes) {

      def decoded[T: ByteCodec]: T = {
        val reader = new VLQByteBufferReader(ByteBuffer.wrap(bytes.toArray))
        ByteCodec[T].decode(reader)
      }
    }
  }

  object instances extends Instances
}
