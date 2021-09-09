package co.topl.utils.codecs.binary

import cats.implicits._
import scodec.{Attempt, DecodeResult, Decoder, Encoder, Err, SizeBound}
import scodec.bits.BitVector

object OptionCodec {

  private val noneBitVector: BitVector = BitVector(0)

  private val someBitVector: BitVector = BitVector(1)

  def decode[T: Decoder](from: BitVector): Attempt[DecodeResult[Option[T]]] =
    if (from.length < byteSize) Attempt.failure(Err.insufficientBits(byteSize, from.length))
    else {
      val (byteBits, remaining) = from.splitAt(byteSize)

      if (byteBits === noneBitVector) Attempt.successful(DecodeResult(None, remaining))
      else Decoder[T].decode(remaining).map(_.map(_.some))
    }

  def encode[T: Encoder](value: Option[T]): Attempt[BitVector] =
    value match {
      case Some(v) => Encoder[T].encode(v).map(someBitVector ++ _)
      case None    => Attempt.successful(noneBitVector)
    }

  trait Implicits {
    implicit def optionDecoder[T: Decoder]: Decoder[Option[T]] = decode[T]

    implicit def optionEncoder[T: Encoder]: Encoder[Option[T]] = new Encoder[Option[T]] {
      override def encode(value: Option[T]): Attempt[BitVector] = OptionCodec.encode[T](value)

      override def sizeBound: SizeBound = Encoder[T].sizeBound + byteSize
    }
  }
}
