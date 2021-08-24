package co.topl.leveldb

import simulacrum.typeclass

@typeclass trait ByteCodec[T] {
  def serialize(t:       T): Array[Byte]
  def deserialize(bytes: Array[Byte]): T
}

object ByteCodec {

  object Instances {

    implicit val byteArraySerializer: ByteCodec[Array[Byte]] =
      new ByteCodec[Array[Byte]] {
        override def serialize(t: Array[Byte]): Array[Byte] = t

        override def deserialize(bytes: Array[Byte]): Array[Byte] = bytes
      }
  }
}
