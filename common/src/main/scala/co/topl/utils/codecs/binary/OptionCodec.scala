package co.topl.utils.codecs.binary

import cats.implicits._
import scodec.{Attempt, Codec, DecodeResult, Decoder, Encoder, Err, SizeBound}
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

  def codec[T: Codec]: Codec[Option[T]] = new Codec[Option[T]] {
    override def decode(bits: BitVector): Attempt[DecodeResult[Option[T]]] = OptionCodec.decode(bits)

    override def encode(value: Option[T]): Attempt[BitVector] = OptionCodec.encode(value)

    override def sizeBound: SizeBound = Codec[T].sizeBound + SizeBound.exact(byteSize)
  }

  trait Codecs {
    def option[T: Codec]: Codec[Option[T]] = OptionCodec.codec
  }

  trait Implicits {
    implicit def optionImplicitCodec[T: Codec]: Codec[Option[T]] = OptionCodec.codec
  }
}
