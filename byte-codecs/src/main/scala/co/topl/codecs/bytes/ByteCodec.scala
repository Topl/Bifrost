package co.topl.codecs.bytes

import co.topl.models.Bytes
import simulacrum.{op, typeclass}

import java.nio.ByteBuffer

@typeclass trait ByteCodec[T] {
  @op("writeBytesTo") def encode(t: T, writer: Writer): Unit
  def decode(reader:                Reader): T
}

object ByteCodec {

  trait Implicits {
    import ByteCodec.ops._

    implicit class TOps[T: ByteCodec](t: T) {

      def bytes: Bytes = {
        val writer = new VLQByteBufferWriter(new ByteArrayBuilder())
        t.writeBytesTo(writer)
        Bytes(writer.toBytes)
      }
    }

    implicit class ArraySeqOps(bytes: Bytes) {

      def decoded[T: ByteCodec]: T = {
        val reader = new VLQByteBufferReader(ByteBuffer.wrap(bytes.toArray))
        ByteCodec[T].decode(reader)
      }
    }
  }

  object Instances {
    implicit def listCodec[T: ByteCodec]: ByteCodec[List[T]] = ???
  }

  object implicits extends Implicits
}
