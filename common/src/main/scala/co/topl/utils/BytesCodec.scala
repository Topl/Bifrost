package co.topl.utils

import cats.data.ValidatedNec
import cats.implicits._

import scala.language.implicitConversions

trait ToBytes[Decoded] {
  def encode(decoded: Decoded): Array[Byte]
}

object ToBytes {

  class Ops[T](val instance: T) extends AnyVal {

    def encodeToBytes(implicit encoder: ToBytes[T]): Array[Byte] =
      encoder.encode(instance)
  }

  trait ToOps {
    implicit def toEncoderOps[T](target: T): Ops[T] = new Ops(target)
  }

  trait Instances {
    val identityBytesEncoder: ToBytes[Array[Byte]] = identity
  }

  object implicits extends ToOps with Instances
}

trait FromBytes[DecodeFailure, Decoded] {
  def decode(encoded: Array[Byte]): ValidatedNec[DecodeFailure, Decoded]
}

object FromBytes {

  class Ops(val instance: Array[Byte]) extends AnyVal {

    def decodeTo[DecodeFailure, Decoded](implicit
      decoder: FromBytes[DecodeFailure, Decoded]
    ): ValidatedNec[DecodeFailure, Decoded] =
      decoder.decode(instance)
  }

  trait ToOps {
    implicit def toDecoderOps(target: Array[Byte]): Ops = new Ops(target)
  }

  trait Instances {
    val identityBytesDecoder: FromBytes[Nothing, Array[Byte]] = bytes => bytes.validNec[Nothing]
  }

  object implicits extends ToOps with Instances
}
