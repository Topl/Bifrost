package co.topl.codecs.bytes.scodecs.ops

import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}

import scala.language.implicitConversions

final class ScodecOps[T](private val codec: Codec[T]) extends AnyVal {

  /**
   * Maps an error with decoding using the [[Codec]] to another error type.
   * @param fErr the mapping function to generate the new error
   * @return a codec with a new error message on decoding failure
   */
  def mapDecodeErr(fErr: Err => Err): Codec[T] =
    new Codec[T] {
      override def encode(value: T): Attempt[BitVector] = codec.encode(value)
      override def sizeBound: SizeBound = codec.sizeBound
      override def decode(bits: BitVector): Attempt[DecodeResult[T]] = codec.decode(bits).mapErr(fErr)
    }

  /**
   * Maps an error with encoding using the [[Codec]] to another error type.
   * @param fErr the mapping function to generate the new error
   * @return a codec with a new error message on encoding failure
   */
  def mapEncodeErr(fErr: Err => Err): Codec[T] =
    new Codec[T] {
      override def encode(value: T): Attempt[BitVector] = codec.encode(value).mapErr(fErr)
      override def sizeBound: SizeBound = codec.sizeBound
      override def decode(bits: BitVector): Attempt[DecodeResult[T]] = codec.decode(bits)
    }
}

object ScodecOps {

  trait ToScodecOps {
    implicit def fromCodec[T](codec: Codec[T]): ScodecOps[T] = new ScodecOps[T](codec)
  }
}
