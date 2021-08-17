package co.topl.codecs.bytes

import co.topl.models.Bytes

trait ByteCodec[T] {
  def encode(t:      T, writer: ByteWriter): ByteWriter
  def decode(reader: ByteReader): (T, ByteReader)
}

object ByteCodec {

  trait Implicits {

    implicit class TOps[T: ByteCodec](t: T) {
      def bytes: Bytes = ???
    }

    implicit class ArraySeqOps(bytes: Bytes) {
      def decoded[T: ByteCodec]: T = ???
    }
  }

  object implicits extends Implicits
}

object ByteCodecInstances {
  implicit def listCodec[T: ByteCodec]: ByteCodec[List[T]] = ???
}

trait ByteReader {}

trait ByteWriter {}
