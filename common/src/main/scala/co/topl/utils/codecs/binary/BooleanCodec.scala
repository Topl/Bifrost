package co.topl.utils.codecs.binary

import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult, Decoder, Encoder, Err, SizeBound}

object BooleanCodec {

  private val trueByte: Byte = 0x01
  private val trueByteBitVector: BitVector = BitVector(trueByte)

  private val falseByte: Byte = 0x00
  private val falseByteBitVector: BitVector = BitVector(falseByte)

  def decode(from: BitVector): Attempt[DecodeResult[Boolean]] =
    if (from.length < byteSize) Attempt.failure(Err.insufficientBits(byteSize, from.length))
    else {
      val (headByteBits, remainder) = from.splitAt(byteSize)
      Attempt.successful(DecodeResult(headByteBits === trueByteBitVector, remainder))
    }

  def encode(value: Boolean): Attempt[BitVector] =
    if (value) Attempt.successful(trueByteBitVector)
    else Attempt.successful(falseByteBitVector)

  trait Implicits {

    implicit val booleanDecoder: Decoder[Boolean] = decode

    implicit val booleanEncoder: Encoder[Boolean] = new Encoder[Boolean] {
      override def encode(value: Boolean): Attempt[BitVector] = BooleanCodec.encode(value)

      override def sizeBound: SizeBound = SizeBound.exact(byteSize)
    }

    implicit val booleanCodec: Codec[Boolean] = Codec(booleanEncoder, booleanDecoder)
  }
}
